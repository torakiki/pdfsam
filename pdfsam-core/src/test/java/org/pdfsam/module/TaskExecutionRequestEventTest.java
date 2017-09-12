/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2012
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

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.sejda.model.parameter.base.AbstractParameters;

/**
 * @author Andrea Vacondio
 * 
 */
@RunWith(value = org.mockito.runners.MockitoJUnitRunner.class)
public class TaskExecutionRequestEventTest {
    @Mock
    private AbstractParameters params;

    @Test(expected = IllegalArgumentException.class)
    public void testNullParams() {
        new TaskExecutionRequestEvent("id", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullId() {
        new TaskExecutionRequestEvent(null, params);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNull() {
        new TaskExecutionRequestEvent(null, null);
    }

    @Test
    public void testNotNull() {
        TaskExecutionRequestEvent victim = new TaskExecutionRequestEvent("id", params);
        Assert.assertEquals(params, victim.getParameters());
        Assert.assertEquals("id", victim.getModuleId());
    }

}
