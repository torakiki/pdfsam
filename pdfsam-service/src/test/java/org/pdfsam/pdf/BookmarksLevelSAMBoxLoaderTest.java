/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2015
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
package org.pdfsam.pdf;

import static org.hamcrest.Matchers.hasItems;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.sejda.common.ComponentsUtility;
import org.sejda.io.SeekableSources;
import org.sejda.sambox.input.PDFParser;
import org.sejda.sambox.pdmodel.PDDocument;

/**
 * @author Andrea Vacondio
 *
 */
public class BookmarksLevelSAMBoxLoaderTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private PDDocument document;
    private PdfDocumentDescriptor descriptor;

    @Before
    public void setUp() throws IOException {
        descriptor = mock(PdfDocumentDescriptor.class);
        document = PDFParser
                .parse(SeekableSources.inMemorySeekableSourceFrom(getClass().getResourceAsStream("/test_outline.pdf")));
    }

    @After
    public void tearDown() {
        ComponentsUtility.nullSafeCloseQuietly(document);
    }

    @Test
    public void accept() {
        new BookmarksLevelSAMBoxLoader().accept(document, descriptor);
        ArgumentCaptor<Set> captor = ArgumentCaptor.forClass(Set.class);
        verify(descriptor).setValidBookmarksLevels(captor.capture());
        assertThat((Set<Integer>) captor.getValue(), hasItems(1, 2, 3));
    }
}
