/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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
package org.pdfsam.ui.selection.multiple;

import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.util.Arrays;

import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class IntegerColumnTest {

    @Test
    public void comparators() {
        Arrays.stream(IntColumn.values()).forEach(v -> assertEquals(-1, v.comparator().compare(1, 10)));
    }

    @Test
    public void getNullTextValue() {
        Arrays.stream(IntColumn.values()).forEach(v -> assertThat(v.getTextValue(null), isEmptyString()));
    }

    @Test
    public void negativePages() {
        assertThat(IntColumn.PAGES.getTextValue(-1), isEmptyString());
    }

    @Test
    public void pages() {
        assertEquals("10", IntColumn.PAGES.getTextValue(10));
    }

    @Test
    public void getPagesObservableValue() {
        File file = mock(File.class);
        SelectionTableRowData data = new SelectionTableRowData(file);
        data.pages(10);
        assertEquals(10, IntColumn.PAGES.getObservableValue(data).getValue().intValue());
    }
}
