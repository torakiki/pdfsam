/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/mar/2015
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
package org.pdfsam.pdfbox.component;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.pdfbox.component.PdfRotator.applyRotation;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.junit.Test;
import org.sejda.model.rotation.PageRotation;
import org.sejda.model.rotation.Rotation;
import org.sejda.model.rotation.RotationType;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfRotatorTest {

    @Test
    public void singlePage() {
        PDDocument document = mock(PDDocument.class);
        PDPage page = mock(PDPage.class);
        when(page.getRotation()).thenReturn(180);
        when(document.getPage(2)).thenReturn(page);
        PageRotation rotation = PageRotation.createSinglePageRotation(3, Rotation.DEGREES_270);
        applyRotation(rotation).to(document);
        verify(page).setRotation(90);
    }

    @Test
    public void multiplePages() {
        PDDocument document = mock(PDDocument.class);
        PDPage page1 = mock(PDPage.class);
        when(page1.getRotation()).thenReturn(180);
        when(document.getPage(0)).thenReturn(page1);
        PDPage page2 = mock(PDPage.class);
        when(page2.getRotation()).thenReturn(90);
        when(document.getPage(1)).thenReturn(page2);
        when(document.getNumberOfPages()).thenReturn(2);
        PageRotation rotation = PageRotation.createMultiplePagesRotation(Rotation.DEGREES_270, RotationType.ALL_PAGES);
        applyRotation(rotation).to(document);
        verify(page1).setRotation(90);
        verify(page2).setRotation(0);
    }
}
