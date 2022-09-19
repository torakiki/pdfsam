/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/mag/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.text.DateFormat;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.sejda.model.pdf.PdfMetadataFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.scene.control.Label;

/**
 * Tab displaying a summary of the PDF document information.
 * 
 * @author Andrea Vacondio
 *
 */
class SummaryTab extends BaseInfoTab implements ChangeListener<PdfDescriptorLoadingStatus> {
    private static final Logger LOG = LoggerFactory.getLogger(SummaryTab.class);
    private static FastDateFormat FORMATTER = FastDateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.MEDIUM);

    private Label fileLabel = createValueLabel();
    private Label size = createValueLabel();
    private Label version = createValueLabel();
    private Label created = createValueLabel();
    private Label modified = createValueLabel();
    private Label pages = createValueLabel();
    private Label title = createValueLabel();
    private Label author = createValueLabel();
    private Label creator = createValueLabel();
    private Label producer = createValueLabel();
    private Label subject = createValueLabel();
    private PdfDocumentDescriptor current;

    SummaryTab() {
        setText(i18n().tr("Summary"));
        grid().add(createTitleLabel("File"), 0, 0);
        grid().add(fileLabel, 1, 0);
        grid().add(createTitleLabel("Size"), 0, 1);
        grid().add(size, 1, 1);
        grid().add(createTitleLabel("Created"), 0, 2);
        grid().add(created, 1, 2);
        grid().add(createTitleLabel("Modified"), 0, 3);
        grid().add(modified, 1, 3);
        grid().add(createTitleLabel("PDF version"), 0, 4);
        grid().add(version, 1, 4);
        grid().add(createTitleLabel("Pages"), 0, 5);
        grid().add(pages, 1, 5);
        grid().add(createTitleLabel("Title"), 0, 6);
        grid().add(title, 1, 6);
        grid().add(createTitleLabel("Author"), 0, 7);
        grid().add(author, 1, 7);
        grid().add(createTitleLabel("Creator"), 0, 8);
        grid().add(creator, 1, 8);
        grid().add(createTitleLabel("Producer"), 0, 9);
        grid().add(producer, 1, 9);
        grid().add(createTitleLabel("Subject"), 0, 10);
        grid().add(subject, 1, 10);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    void requestShow(ShowPdfDescriptorRequest event) {
        if (current != event.getDescriptor()) {
            current = event.getDescriptor();
            current.loadingStatus().addListener(new WeakChangeListener<>(this));
        }
        setFileProperties(current.getFile());
        setPdfProperties();
    }

    private void setFileProperties(File file) {
        fileLabel.setText(file.getAbsolutePath());
        size.setText(FileUtils.byteCountToDisplaySize(file.length()));
        modified.setText(FORMATTER.format(file.lastModified()));
    }

    private void setPdfProperties() {
        version.setText(current.getVersionString());
        pages.setText(Integer.toString(current.pages().getValue()));
        created.setText(current.getInformation("FormattedCreationDate"));
        title.setText(current.getInformation(PdfMetadataFields.TITLE));
        author.setText(current.getInformation(PdfMetadataFields.AUTHOR));
        creator.setText(current.getInformation(PdfMetadataFields.CREATOR));
        subject.setText(current.getInformation(PdfMetadataFields.SUBJECT));
        producer.setText(current.getInformation("Producer"));
    }

    @Override
    public void changed(ObservableValue<? extends PdfDescriptorLoadingStatus> observable,
            PdfDescriptorLoadingStatus oldValue, PdfDescriptorLoadingStatus newValue) {
        if (newValue == PdfDescriptorLoadingStatus.LOADED) {
            LOG.trace("Descriptor loaded, updating summary tab");
            Platform.runLater(() -> setPdfProperties());
        }
    }
}
