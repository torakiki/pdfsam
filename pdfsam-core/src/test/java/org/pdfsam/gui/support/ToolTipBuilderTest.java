/*
 * Created on 07/feb/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
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
        builder.append("");
    }

    @Test
    public void testSingleOutput() {
        ToolTipBuilder builder = new ToolTipBuilder();
        builder.append("ONE");
        assertEquals("<html><body><div>ONE</div></body></html>", builder.toString());
    }

    @Test
    public void testMultipleOutput() {
        ToolTipBuilder builder = new ToolTipBuilder();
        builder.append("ONE");
        builder.append("TWO");
        assertEquals("<html><body><div>ONE<br />TWO</div></body></html>", builder.toString());
    }

    @Test
    public void testEmpty() {
        ToolTipBuilder builder = new ToolTipBuilder();
        assertEquals(EMPTY, builder.toString());
    }

}
