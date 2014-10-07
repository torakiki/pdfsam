/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ott/2014
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
package org.pdfsam.ui;

import org.junit.Test;
import org.pdfsam.TestUtils;

/**
 * @author Andrea Vacondio
 *
 */
public class StageStatusTest {

    @Test(expected = IllegalArgumentException.class)
    public void requiredMode() {
        new StageStatus(1, 1, 1, 1, null);
    }

    @Test
    public void testEquals() {
        StageStatus eq1 = new StageStatus(10, 20, 100, 200, StageMode.MAXIMIZED);
        StageStatus eq2 = new StageStatus(10, 20, 100, 200, StageMode.MAXIMIZED);
        StageStatus eq3 = new StageStatus(10, 20, 100, 200, StageMode.MAXIMIZED);
        StageStatus diff = new StageStatus(20, 10, 200, 100, StageMode.ICONIFIED);
        TestUtils.testEqualsAndHashCodes(eq1, eq2, eq3, diff);
    }
}
