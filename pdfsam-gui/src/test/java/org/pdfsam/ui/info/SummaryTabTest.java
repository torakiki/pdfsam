/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ago/2014
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
package org.pdfsam.ui.info;

import static org.hamcrest.Matchers.isIn;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import javafx.beans.value.ChangeListener;
import javafx.scene.Node;
import javafx.scene.control.Labeled;
import javafx.scene.control.ScrollPane;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.sejda.model.pdf.PdfMetadataKey;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 *
 */
public class SummaryTabTest {
    private static FastDateFormat FORMATTER = FastDateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.MEDIUM);

    @Rule
    public ClearEventStudioRule studio = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Rule
    public InitializeJavaFxThreadRule javaFxThread = new InitializeJavaFxThreadRule();

    @Test
    public void showRequest() throws Exception {
        File file = folder.newFile();
        SummaryTab victim = new SummaryTab();
        Set<Node> properties = ((ScrollPane) victim.getContent()).getContent().lookupAll(".info-property-value");
        assertNotNull(properties);
        assertFalse(properties.isEmpty());
        List<ChangeListener<? super String>> listeners = initListener(properties);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        fillDescriptor(descriptor);
        FXTestUtils.invokeAndWait(() -> victim.requestShow(new ShowPdfDescriptorRequest(descriptor)), 1);
        assertInfoIsDisplayed(listeners, descriptor);
    }

    @Test
    public void onLoad() throws Exception {
        File file = folder.newFile();
        SummaryTab victim = new SummaryTab();
        Set<Node> properties = ((ScrollPane) victim.getContent()).getContent().lookupAll(".info-property-value");
        assertNotNull(properties);
        assertFalse(properties.isEmpty());
        List<ChangeListener<? super String>> listeners = initListener(properties);
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        FXTestUtils.invokeAndWait(() -> victim.requestShow(new ShowPdfDescriptorRequest(descriptor)), 1);
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
        descriptor.putInformation(PdfMetadataKey.TITLE.getKey(), "test.title");
        descriptor.putInformation(PdfMetadataKey.AUTHOR.getKey(), "test.author");
        descriptor.putInformation(PdfMetadataKey.CREATOR.getKey(), "test.creator");
        descriptor.putInformation(PdfMetadataKey.SUBJECT.getKey(), "test.subject");
        descriptor.putInformation("Producer", "test.producer");
        descriptor.putInformation("FormattedCreationDate", "test.creationDate");
        descriptor.setPages(2);
        descriptor.setVersion(PdfVersion.VERSION_1_5);
    }

    private void assertInfoIsDisplayed(List<ChangeListener<? super String>> listeners, PdfDocumentDescriptor descriptor) {
        File file = descriptor.getFile();
        List<String> values = Arrays.asList("test.producer", file.getAbsolutePath(), descriptor.getVersionString(),
                "2", "test.creationDate", "test.title", "test.author", "test.creator", "test.subject",
                FileUtils.byteCountToDisplaySize(file.length()), FORMATTER.format(file.lastModified()));
        listeners.forEach(l -> verify(l, timeout(2000).times(1)).changed(any(), any(), argThat(isIn(values))));
    }
}
