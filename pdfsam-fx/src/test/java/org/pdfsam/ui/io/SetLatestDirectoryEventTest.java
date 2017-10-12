/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/lug/2014
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
package org.pdfsam.ui.io;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import java.io.File;

import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class SetLatestDirectoryEventTest {

    @Test(expected = IllegalArgumentException.class)
    public void nullArg() {
        new SetLatestDirectoryEvent(null);
    }

    @Test
    public void notNullArg() {
        File arg = mock(File.class);
        SetLatestDirectoryEvent victim = new SetLatestDirectoryEvent(arg);
        assertEquals(arg, victim.getLatest());
    }
}
