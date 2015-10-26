/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
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
package org.pdfsam.news;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import org.junit.Before;
import org.junit.Test;
import org.pdfsam.Pdfsam;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultNewsServiceTest {

    private DefaultNewsService victim;

    @Before
    public void setUp() {
        Pdfsam pdfsam = mock(Pdfsam.class);
        victim = new DefaultNewsService(pdfsam);
    }

    @Test
    public void testSetLatestNewsSeen() {
        victim.clear();
        assertEquals(-1, victim.getLatestNewsSeen());
        victim.setLatestNewsSeen(5);
        assertEquals(5, victim.getLatestNewsSeen());
    }

    @Test
    public void testClear() {
        victim.setLatestNewsSeen(5);
        assertEquals(5, victim.getLatestNewsSeen());
        victim.clear();
        assertEquals(-1, victim.getLatestNewsSeen());
    }

}
