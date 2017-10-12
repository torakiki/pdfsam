/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import java.util.concurrent.atomic.AtomicReference;

import javafx.beans.value.ObservableValueBase;

/**
 * An observable value hold as an {@link AtomicReference}
 * 
 * @author Andrea Vacondio
 *
 */
public class ObservableAtomicReference<T> extends ObservableValueBase<T> {

    private AtomicReference<T> value = new AtomicReference<>();

    public ObservableAtomicReference(T initialValue) {
        this.value.set(initialValue);
    }

    @Override
    public T getValue() {
        return value.get();
    }

    public void set(T newValue) {
        value.set(newValue);
        fireValueChangedEvent();
    }

}
