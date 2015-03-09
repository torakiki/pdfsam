/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/mar/2015
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

import static org.mockito.Matchers.anyFloat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.apache.pdfbox.cos.COSDocument;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentCatalog;
import org.apache.pdfbox.pdmodel.PDDocumentInformation;
import org.apache.pdfbox.pdmodel.PageLayout;
import org.apache.pdfbox.pdmodel.PageMode;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDDocumentOutline;
import org.apache.pdfbox.pdmodel.interactive.viewerpreferences.PDViewerPreferences;
import org.junit.Before;
import org.junit.Test;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 *
 */
public class PDDocumentHandlerTest {

    private PDDocument document;
    private COSDocument cosDocument;
    private PDDocumentCatalog catalog;
    private PDDocumentHandler victim;

    @Before
    public void setUp() {
        document = mock(PDDocument.class);
        cosDocument = mock(COSDocument.class);
        catalog = mock(PDDocumentCatalog.class);
        when(document.getDocument()).thenReturn(cosDocument);
        when(document.getDocumentCatalog()).thenReturn(catalog);
        victim = new PDDocumentHandler(document);
    }

    @Test(expected = NullPointerException.class)
    public void nullDocument() {
        new PDDocumentHandler(null);
    }

    @Test
    public void setDocumentInformation() {
        PDDocumentInformation info = mock(PDDocumentInformation.class);
        victim.setDocumentInformation(info);
        verify(document).setDocumentInformation(info);
    }

    @Test
    public void setDocumentOutline() {
        PDDocumentOutline outline = new PDDocumentOutline();
        victim.setDocumentOutline(outline);
        verify(catalog).setDocumentOutline(outline);
    }

    @Test
    public void setPageMode() {
        victim.setPageMode(PageMode.USE_OUTLINES);
        verify(catalog).setPageMode(PageMode.USE_OUTLINES);
    }

    @Test
    public void setPageLayout() {
        victim.setPageLayout(PageLayout.TWO_COLUMN_LEFT);
        verify(catalog).setPageLayout(PageLayout.TWO_COLUMN_LEFT);
    }

    @Test
    public void nullSafeSetVersion() {
        victim.setVersion(null);
    }

    @Test
    public void setVersion() {
        victim.setVersion(PdfVersion.VERSION_1_4);
        verify(document).setVersion(anyFloat());
    }

    @Test
    public void compressXref() {
        victim.compressXrefStream(true);
        verify(cosDocument).setIsXRefStream(true);
    }

    @Test
    public void close() throws IOException {
        victim.close();
        verify(document).close();
    }

    @Test
    public void setViewerPreferences() {
        PDViewerPreferences prefs = mock(PDViewerPreferences.class);
        victim.setViewerPreferences(prefs);
        verify(catalog).setViewerPreferences(prefs);
    }

}
