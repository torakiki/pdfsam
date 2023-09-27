/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.commons;

import javafx.beans.value.ObservableValue;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Andrea Vacondio
 */
public class ToggleChangeListenerTest {

    private final ToggleChangeListener<String> victim = new ToggleChangeListener<>() {
        @Override
        public void onChanged(ObservableValue<? extends String> observable, String oldValue, String newValue) {
            throw new RuntimeException("test");
        }
    };

    @Test
    public void disabled() {
        victim.disabled(true);
        victim.changed(null, null, null);
    }

    @Test
    public void enabled() {
        victim.disabled(false);
        assertThrows(RuntimeException.class, () -> victim.changed(null, null, null), "test");
    }
}
