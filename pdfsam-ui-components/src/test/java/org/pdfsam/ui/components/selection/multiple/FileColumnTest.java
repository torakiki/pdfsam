/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06/ago/2014
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
package org.pdfsam.ui.components.selection.multiple;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;

import java.io.File;
import java.util.Locale;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 *
 */
public class FileColumnTest {
    @BeforeAll
    public static void setUpClass() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Test
    public void getObservableValue() {
        File file = mock(File.class);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        SelectionTableRowData data = new SelectionTableRowData(descriptor);
        assertEquals(file, FileColumn.NAME.getObservableValue(data).getValue());
    }

    @Test
    public void getNullTextValue() {
        assertThat(FileColumn.NAME.getTextValue(null)).isNullOrEmpty();
    }

    @Test
    public void getTextValue() {
        File file = mock(File.class);
        when(file.getName()).thenReturn("name");
        assertEquals("name", FileColumn.NAME.getTextValue(file));
    }

    @Test
    public void comparator() {
        assertTrue(FileColumn.NAME.comparator().compare(new File("_a.pdf"), new File("0.pdf")) < 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("1192name.pdf"), new File("chuck.norris")) < 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("chuck.norris"), new File("1192name.pdf")) > 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("a.pdf"), new File("b.pdf")) < 0);
        assertTrue(FileColumn.NAME.comparator().compare(new File("b.pdf"), new File("a.pdf")) > 0);
        assertEquals(0, FileColumn.NAME.comparator().compare(new File("a.pdf"), new File("a.pdf")));
        assertTrue(FileColumn.NAME.comparator().compare(new File("A.pdf"), new File("b.pdf")) < 0);
        assertEquals(-1, FileColumn.NAME.comparator().compare(new File("1_name"), new File("2_name")));
        assertEquals(-1, FileColumn.NAME.comparator().compare(new File("001_name"), new File("1_name")));
    }
}
