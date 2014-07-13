/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/lug/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.module;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.apache.commons.lang3.StringUtils;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.JavaFXThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class ModuleOwnedButtonTest {
    @Rule
    public JavaFXThreadRule rule = new JavaFXThreadRule();

    @Test
    public void nullArg() {
        ModuleOwnedButton victim = new ModuleOwnedButton(null);
        assertNotNull(victim.getOwnerModule());
        assertEquals(StringUtils.EMPTY, victim.getOwnerModule());
    }

    @Test
    public void notNullArg() {
        ModuleOwnedButton victim = new ModuleOwnedButton("Chuck");
        assertEquals("Chuck", victim.getOwnerModule());
    }
}
