/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ago/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
import javafx.scene.Node;
import javafx.scene.control.Labeled;
import javafx.scene.control.ScrollPane;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.junit.jupiter.api.Test;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.ui.ShowPdfDescriptorRequest;
import org.sejda.model.pdf.PdfMetadataFields;
import org.sejda.model.pdf.PdfVersion;
import org.testfx.util.WaitForAsyncUtils;

import java.io.File;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.collection.IsIn.in;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.hamcrest.MockitoHamcrest.argThat;

/**
 * @author Andrea Vacondio
 */
public class SummaryTabTest {
    private static final FastDateFormat FORMATTER = FastDateFormat.getDateTimeInstance(DateFormat.FULL,
            DateFormat.MEDIUM);

    @Test
    public void showRequest() {
        SummaryTab victim = new SummaryTab();
        Set<Node> properties = ((ScrollPane) victim.getContent()).getContent().lookupAll(".info-property-value");
        assertNotNull(properties);
        assertFalse(properties.isEmpty());
        List<ChangeListener<? super String>> listeners = initListener(properties);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        fillDescriptor(descriptor);
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.requestShow(new ShowPdfDescriptorRequest(descriptor)));
        assertInfoIsDisplayed(listeners, descriptor);
    }

    @Test
    public void onLoad() {
        SummaryTab victim = new SummaryTab();
        Set<Node> properties = ((ScrollPane) victim.getContent()).getContent().lookupAll(".info-property-value");
        assertNotNull(properties);
        assertFalse(properties.isEmpty());
        List<ChangeListener<? super String>> listeners = initListener(properties);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(mock(File.class));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.requestShow(new ShowPdfDescriptorRequest(descriptor)));
        fillDescriptor(descriptor);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        assertInfoIsDisplayed(listeners, descriptor);
    }

    private List<ChangeListener<? super String>> initListener(Set<Node> properties) {
        List<ChangeListener<? super String>> listeners = new ArrayList<>();
        properties.stream().filter(n -> n instanceof Labeled).map(n -> (Labeled) n).forEach(l -> {
            ChangeListener<? super String> listener = mock(ChangeListener.class);
            listeners.add(listener);
            l.textProperty().addListener(listener);
        });
        return listeners;
    }

    private void fillDescriptor(PdfDocumentDescriptor descriptor) {
        descriptor.putInformation(PdfMetadataFields.TITLE, "test.title");
        descriptor.putInformation(PdfMetadataFields.AUTHOR, "test.author");
        descriptor.putInformation(PdfMetadataFields.CREATOR, "test.creator");
        descriptor.putInformation(PdfMetadataFields.SUBJECT, "test.subject");
        descriptor.putInformation("Producer", "test.producer");
        descriptor.putInformation("FormattedCreationDate", "test.creationDate");
        descriptor.pages(2);
        descriptor.setVersion(PdfVersion.VERSION_1_5);
    }

    private void assertInfoIsDisplayed(List<ChangeListener<? super String>> listeners,
            PdfDocumentDescriptor descriptor) {
        File file = descriptor.getFile();
        List<String> values = Arrays.asList("test.producer", file.getAbsolutePath(), descriptor.getVersionString(), "2",
                "test.creationDate", "test.title", "test.author", "test.creator", "test.subject",
                FileUtils.byteCountToDisplaySize(file.length()), FORMATTER.format(file.lastModified()));
        listeners.forEach(l -> verify(l, timeout(2000).times(1)).changed(any(ObservableValue.class), anyString(),
                argThat(is(in(values)))));
    }
}
