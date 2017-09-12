/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/ago/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.module;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;

/**
 * @author Andrea Vacondio
 *
 */
public class ClearUsageControllerTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    private ClearUsageController victim;
    private UsageService service;

    @Before
    public void setUp() {
        this.service = mock(UsageService.class);
        this.victim = new ClearUsageController(service);
    }

    @Test
    public void clear() {
        victim.request(new ClearUsageRequestEvent());
        verify(service).clear();
    }

}
