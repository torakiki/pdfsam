/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ott/2014
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

import static java.time.Duration.ofDays;
import static java.time.Duration.ofMinutes;
import static java.time.Instant.now;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.pdfsam.ui.NewsPolicy.ALWAYS;
import static org.pdfsam.ui.NewsPolicy.NEVER;
import static org.pdfsam.ui.NewsPolicy.ONCE_A_DAY;
import static org.pdfsam.ui.NewsPolicy.ONCE_A_WEEK;

import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class NewsPolicyTest {
    @Test
    public void isTimeToShow() {
        assertTrue(ALWAYS.isTimeToShow(now()));
        assertFalse(NEVER.isTimeToShow(now()));
        assertFalse(ONCE_A_DAY.isTimeToShow(now()));
        assertFalse(ONCE_A_WEEK.isTimeToShow(now()));
        assertTrue(ONCE_A_DAY.isTimeToShow(now().minus(ofDays(1).plus(ofMinutes(1)))));
        assertTrue(ONCE_A_WEEK.isTimeToShow(now().minus(ofDays(7).plus(ofMinutes(1)))));
    }

}
