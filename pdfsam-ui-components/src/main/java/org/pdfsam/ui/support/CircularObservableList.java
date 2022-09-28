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
package org.pdfsam.ui.support;

import static org.sejda.commons.util.RequireUtils.requireArg;

import java.util.Collection;
import java.util.LinkedList;

import javafx.collections.ModifiableObservableListBase;

/**
 * A {@link ModifiableObservableListBase} with size constraints. When at maxCapacity and an element is added, the eldest element is removed.
 * 
 * @author Andrea Vacondio
 *
 */
public class CircularObservableList<E> extends ModifiableObservableListBase<E> {
    private LinkedList<E> wrapped;
    private int maxCapacity;

    public CircularObservableList(int maxCapacity) {
        this.wrapped = new LinkedList<>();
        setMaxCapacity(maxCapacity);
    }

    public void setMaxCapacity(int maxCapacity) {
        requireArg(maxCapacity > 0, "Max capacity must be a positive value");
        this.maxCapacity = maxCapacity;
        houseKeep();
    }

    public int getMaxCapacity() {
        return maxCapacity;
    }

    /**
     * Makes the list fit its limits by removing last items in cases where the list might have exceeded its limits.
     */
    private void houseKeep() {
        while (size() > maxCapacity) {
            remove(0);
        }
    }

    @Override
    public boolean addAll(int index, Collection<? extends E> c) {
        boolean retVal = false;
        try {
            beginChange();
            retVal = wrapped.addAll(index, c);
            nextAdd(index, index + c.size());
            houseKeep();
            modCount++;
        } finally {
            endChange();
        }
        return retVal;
    }

    @Override
    public E get(int index) {
        return wrapped.get(index);
    }

    @Override
    public int size() {
        return wrapped.size();
    }

    @Override
    protected void doAdd(int index, E element) {
        wrapped.add(index, element);
        houseKeep();
    }

    @Override
    protected E doSet(int index, E element) {
        return wrapped.set(index, element);
    }

    @Override
    protected E doRemove(int index) {
        return wrapped.remove(index);
    }

}
