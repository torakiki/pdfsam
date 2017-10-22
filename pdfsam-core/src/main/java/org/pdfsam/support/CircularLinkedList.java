/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/gen/2015
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.support;

import static org.pdfsam.support.RequireUtils.require;

import java.util.Collection;
import java.util.LinkedList;

/**
 * A {@link LinkedList} with size constraints. When at maxCapacity and an element is added, the eldest element is removed.
 * 
 * @author Andrea Vacondio
 *
 */
public class CircularLinkedList<E> extends LinkedList<E> {
    private int maxCapacity;

    public CircularLinkedList(int maxCapacity) {
        setMaxCapacity(maxCapacity);
    }

    public void setMaxCapacity(int maxCapacity) {
        require(maxCapacity > 0, "Max capacity must be a positive value");
        this.maxCapacity = maxCapacity;
        houseKeep();
    }

    public int getMaxCapacity() {
        return maxCapacity;
    }

    public boolean isFull() {
        return size() >= maxCapacity;
    }

    @Override
    public void addFirst(E e) {
        makeRoom();
        super.addFirst(e);
    }

    @Override
    public void addLast(E e) {
        makeRoom();
        super.addLast(e);
    }

    @Override
    public boolean add(E e) {
        makeRoom();
        return super.add(e);
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        boolean retVal = super.addAll(c);
        houseKeep();
        return retVal;
    }

    @Override
    public boolean addAll(int index, Collection<? extends E> c) {
        boolean retVal = super.addAll(index, c);
        houseKeep();
        return retVal;
    }

    @Override
    public void add(int i, E e) {
        super.add(i, e);
        houseKeep();
    }

    /**
     * Makes a space available if the list is already full. Calling this prior the insertion avoids that the list exceeds its limits.
     */
    private void makeRoom() {
        while (isFull()) {
            pollFirst();
        }
    }

    /**
     * Makes the list fit its limits by removing last items in cases where the list might have exceeded its limits.
     */
    private void houseKeep() {
        while (size() > maxCapacity) {
            pollFirst();
        }
    }
}
