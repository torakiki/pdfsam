/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/mag/2014
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

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.scene.AccessibleAttribute;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Tab;
import javafx.scene.layout.VBox;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.ui.ShowPdfDescriptorRequest;
import org.sejda.model.pdf.PdfMetadataFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Tab displaying the keywords of the PDF document.
 * 
 * @author Andrea Vacondio
 *
 */
class KeywordsTab extends Tab implements ChangeListener<PdfDescriptorLoadingStatus> {
    private static final Logger LOG = LoggerFactory.getLogger(KeywordsTab.class);
    private final Label keywords = new Label();
    private PdfDocumentDescriptor current;

    KeywordsTab() {
        var content = new VBox();
        content.getStyleClass().add("info-props");
        setText(i18n().tr("Keywords"));
        setClosable(false);
        keywords.setWrapText(true);
        keywords.getStyleClass().add("info-property-value");
        content.getChildren().add(keywords);
        var scroll = new ScrollPane(content);
        scroll.setFitToHeight(true);
        scroll.setFitToWidth(true);
        setContent(scroll);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    void requestShow(ShowPdfDescriptorRequest event) {
        if (current != event.descriptor()) {
            current = event.descriptor();
            current.loadingStatus().addListener(new WeakChangeListener<>(this));
        }
        keywords.setText(event.descriptor().getInformation(PdfMetadataFields.KEYWORDS));
    }

    @Override
    public void changed(ObservableValue<? extends PdfDescriptorLoadingStatus> observable,
            PdfDescriptorLoadingStatus oldValue, PdfDescriptorLoadingStatus newValue) {
        if (newValue == PdfDescriptorLoadingStatus.LOADED) {
            LOG.trace("Descriptor loaded, updating keywords tab");
            Platform.runLater(() -> {
                keywords.setText(current.getInformation(PdfMetadataFields.KEYWORDS));
                keywords.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT);
            });
        }
    }
}
