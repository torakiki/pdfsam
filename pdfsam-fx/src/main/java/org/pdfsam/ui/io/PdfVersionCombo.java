/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.io;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.Tooltip;
import javafx.util.Callback;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.support.RequireUtils;
import org.pdfsam.ui.io.PdfVersionCombo.PdfVersionComboItem;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.pdf.PdfVersion;


/**
 * Combo box let the user select the pdf version of the generated output documents
 * 
 * @author Andrea Vacondio
 * 
 */
class PdfVersionCombo extends ComboBox<PdfVersionComboItem> implements ModuleOwned {

    private String ownerModule = StringUtils.EMPTY;
    private ObservableList<PdfVersionComboItem> unfilteredItems = FXCollections.observableArrayList();
    private PdfVersionFilter versionsFilter = new PdfVersionFilter();

    public PdfVersionCombo(String ownerModule) {
        this.ownerModule = ownerModule;
        // Arrays.stream(PdfVersion.values()).filter(v -> v.getVersion() > 2).map(PdfVersionComboItem::new)
        // .collect(Collectors.toCollection(FXCollections::observableArrayList));
        // TODO lambda
        for (PdfVersion current : PdfVersion.values()) {
            if (current.getVersion() > 2) {
                unfilteredItems.add(new PdfVersionComboItem(current));
            }
        }
        setCellFactory(new Callback<ListView<PdfVersionComboItem>, ListCell<PdfVersionComboItem>>() {
            @Override
            public ListCell<PdfVersionComboItem> call(ListView<PdfVersionComboItem> p) {
                ListCell<PdfVersionComboItem> cell = new ListCell<PdfVersionComboItem>() {
                    @Override
                    protected void updateItem(PdfVersionComboItem item, boolean bln) {
                        super.updateItem(item, bln);
                        if (item != null) {
                            setText(item.toString());
                            if (item.sourceVersion) {
                                setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                                        "Same as the input document")));
                            }
                        }
                    }
                };
                return cell;
            }
        });
        versionsFilter.requiredProperty().addListener((observable, oldVal, newVal) -> {
            PdfVersionComboItem selected = getSelectionModel().getSelectedItem();
            setItems(unfilteredItems.filtered(t -> t.isHigherOrEqual(newVal.intValue())));
            int selecedIndex = getItems().indexOf(selected);
            if (selecedIndex != -1) {
                getSelectionModel().select(selecedIndex);
            } else {
                getSelectionModel().selectLast();
            }
        });
        versionsFilter.addFilter(-1);
        getSelectionModel().selectLast();
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void onAddPdfVersionConstraint(final AddPdfVersionConstraintEvent event) {
        versionsFilter.addFilter(event.getPdfVersion().getVersion());
    }

    @EventListener
    public void onRemovePdfVersionConstraint(final RemovePdfVersionConstraintEvent event) {
        versionsFilter.removeFilter(event.getPdfVersion().getVersion());
    }

    @EventListener
    public void onChangedSelectedPdfVersion(final ChangedSelectedPdfVersionEvent event) {
        unfilteredItems.forEach(item -> item.setSourceVersion(event.getPdfVersion() == item.getVersion()));
    }

    @EventStation
    public String getOwnerModule() {
        return this.ownerModule;
    }

    /**
     * Item to use as a model for the {@link PdfVersionCombo}
     * 
     * @author Andrea Vacondio
     * 
     */
    public static class PdfVersionComboItem {

        private PdfVersion version;
        private boolean sourceVersion = false;

        public PdfVersionComboItem(PdfVersion version) {
            RequireUtils.requireNotNull(version, "PDF version cannot be null");
            this.version = version;
        }

        public PdfVersion getVersion() {
            return version;
        }

        public void setSourceVersion(boolean sourceVersion) {
            this.sourceVersion = sourceVersion;
        }

        public boolean isHigherOrEqual(int version) {
            return getVersion().getVersion() >= version;
        }

        @Override
        public String toString() {
            String versionString = DefaultI18nContext.getInstance().i18n("Version {0}",
                    Double.toString(version.getVersionAsDouble()));
            if (sourceVersion) {
                return String.format("%s (*)", versionString);
            }
            return versionString;
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
            if (!(other instanceof PdfVersionComboItem)) {
                return false;
            }
            PdfVersionComboItem otherItem = (PdfVersionComboItem) other;
            return new EqualsBuilder().append(version, otherItem.getVersion()).isEquals();
        }
    }
}
