/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
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
package org.pdfsam.ui.components.io;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.AddPdfVersionConstraintEvent;
import org.pdfsam.model.ui.ChangedSelectedPdfVersionEvent;
import org.pdfsam.model.ui.RemovePdfVersionConstraintEvent;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.ui.components.io.PdfVersionCombo.PdfVersionComboItem;
import org.sejda.model.pdf.PdfVersion;

import java.util.Arrays;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Combo box to let the user select the pdf version of the generated output documents
 *
 * @author Andrea Vacondio
 */
class PdfVersionCombo extends ComboBox<PdfVersionComboItem> implements ToolBound, ResettableView {

    private String toolBinding = StringUtils.EMPTY;
    private final ObservableList<PdfVersionComboItem> unfilteredItems = FXCollections.observableArrayList();
    private final PdfVersionFilter versionsFilter = new PdfVersionFilter();
    private final SameAsSourceComboItem sameAsSource = new SameAsSourceComboItem();

    public PdfVersionCombo(String toolBinding) {
        this.toolBinding = toolBinding;

        Arrays.stream(PdfVersion.values()).filter(v -> v.getVersion() > PdfVersion.VERSION_1_2.getVersion())
                .map(DefaultPdfVersionComboItem::new).forEach(unfilteredItems::add);

        versionsFilter.requiredProperty().addListener((observable, oldVal, newVal) -> setFilteredItems(newVal));
        resetView();
        eventStudio().addAnnotatedListeners(this);
    }

    private void setFilteredItems(PdfVersion required) {
        if (nonNull(required)) {
            PdfVersionComboItem selected = getSelectionModel().getSelectedItem();
            setItems(unfilteredItems.filtered(t -> t.isHigherOrEqual(required)));
            int selecedIndex = getItems().indexOf(selected);
            if (selecedIndex != -1) {
                getSelectionModel().select(selecedIndex);
            } else {
                getSelectionModel().selectFirst();
            }
        }
    }

    @EventListener
    public void onAddPdfVersionConstraint(final AddPdfVersionConstraintEvent event) {
        versionsFilter.addFilter(event.pdfVersion());
    }

    @EventListener
    public void onRemovePdfVersionConstraint(final RemovePdfVersionConstraintEvent event) {
        versionsFilter.removeFilter(event.pdfVersion());
    }

    @EventListener
    public void onChangedSelectedPdfVersion(final ChangedSelectedPdfVersionEvent event) {
        sameAsSource.setVersion(event.pdfVersion());
        setFilteredItems(versionsFilter.requiredProperty().get());
    }

    @Override
    @EventStation
    public String toolBinding() {
        return this.toolBinding;
    }

    @Override
    public void resetView() {
        versionsFilter.reset();
        versionsFilter.addFilter(PdfVersion.VERSION_1_2);
    }

    public void enableSameAsSourceItem() {
        unfilteredItems.add(0, sameAsSource);
        getSelectionModel().selectFirst();
    }

    /**
     * Item for a {@link PdfVersionCombo}
     *
     * @author Andrea Vacondio
     */
    public interface PdfVersionComboItem {
        PdfVersion getVersion();

        boolean isHigherOrEqual(PdfVersion version);
    }

    /**
     * Default implementations for items to be used as a model for the {@link PdfVersionCombo}
     * 
     * @author Andrea Vacondio
     * 
     */
    static class DefaultPdfVersionComboItem implements PdfVersionComboItem {

        private final PdfVersion version;

        public DefaultPdfVersionComboItem(PdfVersion version) {
            requireNotNullArg(version, "PDF version cannot be null");
            this.version = version;
        }

        @Override
        public PdfVersion getVersion() {
            return this.version;
        }

        @Override
        public boolean isHigherOrEqual(PdfVersion version) {
            return this.version.getVersion() >= version.getVersion();
        }

        @Override
        public String toString() {
            return i18n().tr("Version {0}", version.getVersionString());
        }

        @Override
        public int hashCode() {
            return new HashCodeBuilder().append(version).toHashCode();
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof DefaultPdfVersionComboItem otherItem)) {
                return false;
            }
            return new EqualsBuilder().append(version, otherItem.version).isEquals();
        }
    }

    /**
     * Combo item to let the user select the same PDF version of the selected input document
     * 
     * @author Andrea Vacondio
     *
     */
    private static class SameAsSourceComboItem implements PdfVersionComboItem {

        private PdfVersion version = PdfVersion.VERSION_1_5;

        void setVersion(PdfVersion version) {
            this.version = ofNullable(version).orElse(PdfVersion.VERSION_1_5);
        }

        @Override
        public PdfVersion getVersion() {
            return version;
        }

        @Override
        public boolean isHigherOrEqual(PdfVersion other) {
            return this.version.getVersion() >= other.getVersion();
        }

        @Override
        public String toString() {
            return i18n().tr("Same as the input document");
        }

    }

}
