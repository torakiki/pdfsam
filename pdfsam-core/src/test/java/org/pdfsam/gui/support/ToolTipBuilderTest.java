/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/feb/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.support;

import org.junit.Test;
import org.pdfsam.gui.support.ToolTipBuilder;

import static org.apache.commons.lang3.StringUtils.EMPTY;

import static org.junit.Assert.assertEquals;

/**
 * @author Andrea Vacondio
 * 
 */
public class ToolTipBuilderTest {

    @Test(expected = IllegalArgumentException.class)
    public void testRequiredLine() {
        ToolTipBuilder builder = new ToolTipBuilder();
        builder.appendLine("");
    }

    @Test
    public void testSingleOutput() {
        ToolTipBuilder builder = new ToolTipBuilder();
        builder.appendLine("ONE");
        assertEquals("<html><body><div>ONE</div></body></html>", builder.toString());
    }

    @Test
    public void testMultipleOutput() {
        ToolTipBuilder builder = new ToolTipBuilder();
        builder.appendLine("ONE");
        builder.appendLine("TWO");
        assertEquals("<html><body><div>ONE<br />TWO</div></body></html>", builder.toString());
    }

    @Test
    public void testEmpty() {
        ToolTipBuilder builder = new ToolTipBuilder();
        assertEquals(EMPTY, builder.toString());
    }

}
