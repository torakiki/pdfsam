/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ago/2014
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
package org.pdfsam.gui.components.info;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.Labeled;
import javafx.scene.control.ScrollPane;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.ui.ShowPdfDescriptorRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;
import org.sejda.model.pdf.PdfMetadataFields;
import org.testfx.util.WaitForAsyncUtils;

import java.io.File;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ JavaFxThreadInitializeExtension.class, ClearEventStudioExtension.class })
public class KeywordsTabTest {

    @Test
    public void showRequest() {
        KeywordsTab victim = new KeywordsTab();
        Labeled keywords = (Labeled) ((ScrollPane) victim.getContent()).getContent().lookup(".info-property-value");
        assertNotNull(keywords);
        ChangeListener<? super String> listener = mock(ChangeListener.class);
        keywords.textProperty().addListener(listener);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        descriptor.putInformation(PdfMetadataFields.KEYWORDS, "test");
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.requestShow(new ShowPdfDescriptorRequest(descriptor)));
        verify(listener, timeout(2000).times(1)).changed(any(ObservableValue.class), anyString(), eq("test"));
    }

    @Test
    public void onLoad() {
        KeywordsTab victim = new KeywordsTab();
        Labeled keywords = (Labeled) ((ScrollPane) victim.getContent()).getContent().lookup(".info-property-value");
        assertNotNull(keywords);
        ChangeListener<? super String> listener = mock(ChangeListener.class);
        keywords.textProperty().addListener(listener);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.requestShow(new ShowPdfDescriptorRequest(descriptor)));
        descriptor.putInformation(PdfMetadataFields.KEYWORDS, "test");
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        verify(listener, timeout(2000).times(1)).changed(any(ObservableValue.class), anyString(), eq("test"));
    }

}
