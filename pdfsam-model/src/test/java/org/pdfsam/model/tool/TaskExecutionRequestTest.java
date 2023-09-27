/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2012
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
package org.pdfsam.model.tool;

import org.junit.jupiter.api.Test;
import org.sejda.model.parameter.base.AbstractParameters;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 */
public class TaskExecutionRequestTest {

    @Test
    public void testNullParams() {
        assertThrows(IllegalArgumentException.class, () -> new TaskExecutionRequest("id", null));
    }

    @Test
    public void testNullId() {
        assertThrows(IllegalArgumentException.class,
                () -> new TaskExecutionRequest(null, mock(AbstractParameters.class)));
    }

    @Test
    public void testNull() {
        assertThrows(IllegalArgumentException.class, () -> new TaskExecutionRequest(null, null));
    }

    @Test
    public void testNotNull() {
        var params = mock(AbstractParameters.class);
        var victim = new TaskExecutionRequest("id", params);
        assertEquals(params, victim.parameters());
        assertEquals("id", victim.toolId());
    }

}
