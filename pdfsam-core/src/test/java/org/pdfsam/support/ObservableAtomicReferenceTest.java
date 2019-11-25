/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;

import javafx.beans.InvalidationListener;
import javafx.beans.value.ChangeListener;

/**
 * @author Andrea Vacondio
 *
 */
public class ObservableAtomicReferenceTest {

    @Test
    public void constructor() {
        ObservableAtomicReference<String> victim = new ObservableAtomicReference<>("Initial");
        assertEquals("Initial", victim.getValue());
    }

    @Test
    public void listeners() {
        ObservableAtomicReference<String> victim = new ObservableAtomicReference<>("Initial");
        ChangeListener<String> listener = mock(ChangeListener.class);
        InvalidationListener invalidationListener = mock(InvalidationListener.class);
        victim.addListener(listener);
        victim.addListener(invalidationListener);
        victim.set("newVal");
        assertEquals("newVal", victim.getValue());
        verify(listener).changed(any(), eq("Initial"), eq("newVal"));
        verify(invalidationListener).invalidated(victim);
    }
}
