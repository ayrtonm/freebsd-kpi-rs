/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2024 Ayrton Muñoz
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

//! The `Box<T>` type for heap allocation.

use crate::malloc::{Malloc, MallocFlags};
use crate::prelude::*;
use core::cmp::PartialEq;
use core::ffi::c_void;
use core::fmt::{Debug, Formatter};
use core::marker::PhantomData;
use core::mem::{forget, size_of};
use core::ops::{Deref, DerefMut};
use core::ptr::{NonNull, drop_in_place};
use core::{fmt, slice};

/// A pointer to something on the heap.
///
/// When a `Box<T>` is dropped, the T on the heap is deallocated. The memory layout of this type is
/// equivalent to `void *ptr` in C.
#[repr(C)]
pub struct Box<T: ?Sized, M: Malloc = M_DEVBUF>(pub(crate) NonNull<T>, PhantomData<*mut M>);

// impl Deref to allow using a Box<T> like a T transparently
impl<T: ?Sized, M: Malloc> Deref for Box<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T: ?Sized, M: Malloc> DerefMut for Box<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<T: Debug + ?Sized, M: Malloc> Debug for Box<T, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Use Box<T>'s Deref impl since it returns a &T
        Debug::fmt(self.deref(), f)
    }
}

impl<T: PartialEq + ?Sized, M: Malloc> PartialEq for Box<T, M> {
    fn eq(&self, other: &Self) -> bool {
        // Use Box<T>'s Deref impl since it returns a &T
        PartialEq::eq(self.deref(), other)
    }
}

impl<T: Eq + ?Sized, M: Malloc> Eq for Box<T, M> {}

unsafe impl<T: Sync + ?Sized, M: Malloc> Sync for Box<T, M> {}
unsafe impl<T: Send + ?Sized, M: Malloc> Send for Box<T, M> {}

impl<T: ?Sized, M: Malloc> Drop for Box<T, M> {
    fn drop(&mut self) {
        let ptr = self.0.as_ptr();
        // Drop everything that the T owns
        unsafe { drop_in_place(ptr) }
        // Deallocate the memory for the T
        unsafe { free(ptr.cast::<c_void>(), M::malloc_type()) }
    }
}

impl<T, M: Malloc> Box<T, M> {
    pub fn new(t: T, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(t, flags).unwrap()
    }

    pub fn try_new(t: T, flags: MallocFlags) -> Result<Self> {
        if flags.contains(M_NOWAIT) && flags.contains(M_WAITOK) {
            return Err(EDOOFUS);
        };
        let t_size = size_of::<T>();
        let t_align = align_of::<T>();
        let void_ptr = malloc_aligned(t_size, t_align, M::malloc_type(), flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let boxed_t_ptr = nonnull_void_ptr.cast::<T>();
                unsafe { boxed_t_ptr.write(t) };
                Ok(Self(boxed_t_ptr, PhantomData))
            }
            None => Err(ENOMEM),
        }
    }
}

impl<T: ?Sized, M: Malloc> Box<T, M> {
    pub fn into_raw(b: Self) -> *mut T {
        let res = b.0.as_ptr();
        forget(b);
        res
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(NonNull::new(ptr).unwrap(), PhantomData)
    }
}

impl<'a, T, M: Malloc> IntoIterator for &'a Box<[T], M> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;
    fn into_iter(self) -> <&'a Box<[T], M> as IntoIterator>::IntoIter {
        // Rely on Deref<[T]> to use core::slice impl
        self.iter()
    }
}

impl<'a, T, M: Malloc> IntoIterator for &'a mut Box<[T], M> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;
    fn into_iter(self) -> <&'a mut Box<[T], M> as IntoIterator>::IntoIter {
        // Rely on DerefMut<[T]> to use core::slice impl
        self.iter_mut()
    }
}

#[derive(Debug)]
pub struct LinkedList<T, M: Malloc = M_DEVBUF> {
    head: Option<NonNull<Node<T>>>,
    tail: Option<NonNull<Node<T>>>,
    len: usize,
    _marker: PhantomData<*mut M>,
}

impl<T, M: Malloc> Default for LinkedList<T, M> {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl<T: Sync, M: Malloc> Sync for LinkedList<T, M> {}
unsafe impl<T: Send, M: Malloc> Send for LinkedList<T, M> {}

impl<T, M: Malloc> LinkedList<T, M> {
    pub const fn new() -> Self {
        Self {
            head: None,
            tail: None,
            len: 0,
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    fn head_as_mut(&mut self) -> Option<&mut Node<T>> {
        unsafe { self.head.map(|mut ptr| ptr.as_mut()) }
    }

    fn tail_as_mut(&mut self) -> Option<&mut Node<T>> {
        unsafe { self.tail.map(|mut ptr| ptr.as_mut()) }
    }

    pub fn push_back(&mut self, elt: T, flags: MallocFlags) {
        self.len += 1;
        let mut node = Node::new(elt);
        node.prev = self.tail;
        let boxed_node = Box::<Node<T>, M>::new(node, flags);
        let new_node_ptr = NonNull::new(Box::into_raw(boxed_node)).unwrap();
        if self.tail.is_none() && self.head.is_none() {
            self.head = Some(new_node_ptr);
            self.tail = Some(new_node_ptr);
        } else if let Some(old_tail) = self.tail_as_mut() {
            old_tail.next = Some(new_node_ptr);
            self.tail = Some(new_node_ptr);
        };
    }

    pub fn push_front(&mut self, elt: T, flags: MallocFlags) {
        self.len += 1;
        let mut node = Node::new(elt);
        node.next = self.head;
        let boxed_node = Box::<Node<T>, M>::new(node, flags);
        let new_node_ptr = NonNull::new(Box::into_raw(boxed_node)).unwrap();
        if self.tail.is_none() && self.head.is_none() {
            self.head = Some(new_node_ptr);
            self.tail = Some(new_node_ptr);
        } else if let Some(old_head) = self.head_as_mut() {
            old_head.prev = Some(new_node_ptr);
            self.head = Some(new_node_ptr);
        };
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F>(&mut self, mut filter: F) -> bool
    where
        F: FnMut(&mut T) -> bool,
    {
        // Handle no element case
        if self.head.is_none() && self.tail.is_none() {
            return false;
        }
        // Handle single element case
        if self.head.unwrap() == self.tail.unwrap() {
            let keep = filter(&mut self.head_as_mut().unwrap().elt);
            if !keep {
                // Recreate the Box so it gets dropped (i.e. freed)
                let boxed_node: Box<Node<T>, M> =
                    unsafe { Box::from_raw(self.head.unwrap().as_ptr()) };
                self.len -= 1;
                self.head = None;
                self.tail = None;
                return true;
            }
            return false;
        }
        // Handle two or more element case
        let mut cur_node = self.head;
        let mut prev_node: Option<NonNull<Node<T>>> = None;
        let mut next_node = self.head_as_mut().unwrap().next;
        let mut removed_any = false;

        while let Some(mut cur) = cur_node {
            next_node = unsafe { cur.as_ref().next };
            let keep = filter(unsafe { &mut cur.as_mut().elt });
            if !keep {
                removed_any = true;
                self.len -= 1;
                let boxed_node: Box<Node<T>, M> = unsafe { Box::from_raw(cur.as_ptr()) };
                if let Some(mut prev) = prev_node {
                    unsafe {
                        prev.as_mut().next = next_node;
                    }
                };
                if cur_node == self.head {
                    self.head = next_node;
                }
                if cur_node == self.tail {
                    self.tail = prev_node;
                    return removed_any;
                }
                if let Some(mut next) = next_node {
                    unsafe {
                        next.as_mut().prev = prev_node;
                    }
                };
            } else {
                prev_node = cur_node;
            }
            cur_node = next_node;
        }
        return removed_any;
    }
}

#[derive(Debug, Default)]
struct Node<T> {
    next: Option<NonNull<Node<T>>>,
    prev: Option<NonNull<Node<T>>>,
    elt: T,
}

impl<T> Node<T> {
    pub fn new(elt: T) -> Self {
        Self {
            next: None,
            prev: None,
            elt,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::LoudDrop;

    #[test]
    fn boxed() {
        let value = 42;
        let x: Box<u64, M_DEVBUF> = Box::try_new(value, M_NOWAIT).unwrap();
        assert_eq!(*x, value);
        println!("{x:?}");
    }

    #[test]
    fn bad_flags() {
        let x: Result<Box<u32>> = Box::try_new(0xdeadbeefu32, M_NOWAIT | M_WAITOK);
        assert!(x == Err(EDOOFUS));
    }

    #[test]
    fn oom() {
        let x: Result<Box<u8>> = Box::try_new(111u8, M_USE_RESERVE);
        assert!(x == Err(ENOMEM));
    }

    #[test]
    fn round_trip_drop() {
        let y: Box<LoudDrop> = Box::try_new(LoudDrop, M_NOWAIT).unwrap();
        let y_ptr = Box::into_raw(y);
        let _new_y: Box<LoudDrop> = unsafe { Box::from_raw(y_ptr) };
    }

    #[test]
    fn retain_none_1_elt() {
        let mut list: LinkedList<u32, M_DEVBUF> = LinkedList::new();
        assert_eq!(list.len(), 0);
        list.push_back(1u32, M_WAITOK);
        assert_eq!(list.len(), 1);
        list.retain(|_| false);
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn linked_list() {
        let mut list: LinkedList<u32, M_DEVBUF> = LinkedList::new();
        list.push_back(1u32, M_WAITOK);
        list.push_back(2u32, M_WAITOK);
        list.push_back(3u32, M_WAITOK);
        assert_eq!(list.len(), 3);
        list.retain(|x| *x < 2);
        assert_eq!(list.len(), 1);
    }
}
