/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Spliterator;

public class EnhancedLinkedList<E> implements List<E>, Deque<E>, Serializable {

	private static final long serialVersionUID = 5079345907389901541L;

	private final LinkedList<E> linkedList = new LinkedList<>();

	/**
	 * Constructs an empty list.
	 */
	public EnhancedLinkedList() {
	}

	/**
	 * Constructs a list containing the elements of the specified
	 * collection, in the order they are returned by the collection's
	 * iterator.
	 *
	 * @param collection
	 * 		the collection whose elements are to be placed into this list
	 *
	 * @throws NullPointerException
	 * 		if the specified collection is null
	 */
	public EnhancedLinkedList(final Collection<? extends E> collection) {
		linkedList.addAll(collection);
	}

	public EnhancedLinkedList<E> getAllButFirst() {
		final EnhancedLinkedList<E> allButFirst = new EnhancedLinkedList<>(this);
		allButFirst.removeFirst();
		return allButFirst;
	}

	public EnhancedLinkedList<E> getAllButLast() {
		final EnhancedLinkedList<E> allButLast = new EnhancedLinkedList<>(this);
		allButLast.removeLast();
		return allButLast;
	}

	public EnhancedLinkedList<E> getAllButFirstN(final int n) {
		final EnhancedLinkedList<E> allButFirstN = new EnhancedLinkedList<>(this);

		for (int i = 0; i < n; i++) {
			allButFirstN.removeFirst();
		}

		return allButFirstN;
	}

	@Override
	public void addFirst(final E e) {
		linkedList.addFirst(e);
	}

	@Override
	public void addLast(final E e) {
		linkedList.addLast(e);
	}

	@Override
	public boolean offerFirst(final E e) {
		return linkedList.offerFirst(e);
	}

	@Override
	public boolean offerLast(final E e) {
		return linkedList.offerLast(e);
	}

	@Override
	public E removeFirst() {
		// Use pollFirst so we can avoid hitting the NoSuchElementException. We don't want to hit this.
		return linkedList.pollFirst();
	}

	@Override
	public E removeLast() {
		// Use pollLast so we can avoid hitting the NoSuchElementException. We don't want to hit this.
		return linkedList.pollLast();
	}

	@Override
	public E pollFirst() {
		return linkedList.pollFirst();
	}

	@Override
	public E pollLast() {
		return linkedList.pollLast();
	}

	@Override
	public E getFirst() {
		// Use peekFirst so we can avoid hitting the NoSuchElementException. We don't want to hit this.
		return linkedList.peekFirst();
	}

	@Override
	public E getLast() {
		// Use peekLast so we can avoid hitting the NoSuchElementException. We don't want to hit this.
		return linkedList.peekLast();
	}

	@Override
	public E peekFirst() {
		return linkedList.peekFirst();
	}

	@Override
	public E peekLast() {
		return linkedList.peekLast();
	}

	@Override
	public boolean removeFirstOccurrence(final Object o) {
		return linkedList.removeFirstOccurrence(o);
	}

	@Override
	public boolean removeLastOccurrence(final Object o) {
		return linkedList.removeLastOccurrence(o);
	}

	@Override
	public boolean offer(final E e) {
		return linkedList.offer(e);
	}

	@Override
	public E remove() {
		return linkedList.remove();
	}

	@Override
	public E poll() {
		return linkedList.poll();
	}

	@Override
	public E element() {
		// Use peekFirst so we can avoid hitting the NoSuchElementException. We don't want to hit this.
		return linkedList.peekFirst();
	}

	@Override
	public E peek() {
		return linkedList.peek();
	}

	@Override
	public void push(final E e) {
		linkedList.push(e);
	}

	@Override
	public E pop() {
		// Use pollFirst so we can avoid hitting the NoSuchElementException. We don't want to hit this.
		return linkedList.pollFirst();
	}

	@Override
	public Iterator<E> descendingIterator() {
		return linkedList.descendingIterator();
	}

	@Override
	public int size() {
		return linkedList.size();
	}

	@Override
	public boolean isEmpty() {
		return linkedList.isEmpty();
	}

	@Override
	public boolean contains(final Object o) {
		return linkedList.contains(o);
	}

	@Override
	public Iterator<E> iterator() {
		return linkedList.iterator();
	}

	@Override
	public Object[] toArray() {
		return linkedList.toArray();
	}

	@Override
	public <T> T[] toArray(final T[] a) {
		return linkedList.toArray(a);
	}

	@Override
	public boolean add(final E e) {
		return linkedList.add(e);
	}

	@Override
	public boolean remove(final Object o) {
		return linkedList.remove(o);
	}

	@Override
	public boolean containsAll(final Collection<?> c) {
		return linkedList.containsAll(c);
	}

	@Override
	public boolean addAll(final Collection<? extends E> c) {
		return linkedList.addAll(c);
	}

	@Override
	public boolean addAll(final int index, final Collection<? extends E> c) {
		return linkedList.addAll(index, c);
	}

	@Override
	public boolean removeAll(final Collection<?> c) {
		return linkedList.removeAll(c);
	}

	@Override
	public boolean retainAll(final Collection<?> c) {
		return linkedList.retainAll(c);
	}

	@Override
	public void clear() {
		linkedList.clear();
	}

	@Override
	public E get(final int index) {
		return linkedList.get(index);
	}

	@Override
	public E set(final int index, final E element) {
		return linkedList.set(index, element);
	}

	@Override
	public void add(final int index, final E element) {
		linkedList.add(index, element);
	}

	@Override
	public E remove(final int index) {
		return linkedList.remove(index);
	}

	@Override
	public int indexOf(final Object o) {
		return linkedList.indexOf(o);
	}

	@Override
	public int lastIndexOf(final Object o) {
		return linkedList.lastIndexOf(o);
	}

	@Override
	public ListIterator<E> listIterator() {
		return linkedList.listIterator();
	}

	@Override
	public ListIterator<E> listIterator(final int index) {
		return linkedList.listIterator(index);
	}

	@Override
	public List<E> subList(final int fromIndex, final int toIndex) {
		return linkedList.subList(fromIndex, toIndex);
	}

	@Override
	public Spliterator<E> spliterator() {
		return linkedList.spliterator();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
