/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/feb/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.view.output;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JComboBox;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.event.AddPdfVersionConstraintEvent;
import org.pdfsam.gui.event.ChangedSelectedPdfVersionEvent;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.RemovePdfVersionConstraintEvent;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.support.RequireUtils;
import org.sejda.model.pdf.PdfVersion;

/**
 * Combo box displaying PDF versions.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfVersionCombo extends JComboBox implements WithEventNamespace {
    private EventNamespace eventNamespace = EventNamespace.NULL;
    private Set<Integer> filters = new HashSet<Integer>();

    public PdfVersionCombo() {
        for (PdfVersion current : PdfVersion.values()) {
            if (current.getVersion() > 2) {
                addItem(new PdfVersionComboItem(current));
            }
        }
        setSelectedIndex(getItemCount() - 1);
        filters.add(-1);
        AnnotationProcessor.process(this);
    }

    @EventSubscriber
    public void onAddPdfVersionConstraint(AddPdfVersionConstraintEvent event) {
        if (event.getNamespace().isParentOf(eventNamespace)) {
            addFilter(event.getPdfVersion().getVersion());
        }
    }

    @EventSubscriber
    public void onRemovePdfVersionConstraint(RemovePdfVersionConstraintEvent event) {
        if (event.getNamespace().isParentOf(eventNamespace)) {
            removeFilter(event.getPdfVersion().getVersion());
        }
    }

    @EventSubscriber
    public void onChangedSelectedPdfVersion(ChangedSelectedPdfVersionEvent event) {
        if (event.getNamespace().isParentOf(eventNamespace)) {
            for (int i = 0; i < getItemCount(); i++) {
                PdfVersionComboItem item = (PdfVersionComboItem) getModel().getElementAt(i);
                if (event.hasPdfVersion()) {
                    item.setSourceVersion(event.getPdfVersion() == item.getVersion());
                } else {
                    item.setSourceVersion(false);
                }
            }
            repaint();
        }
    }

    public void addFilter(Integer version) {
        // the filter is not already there
        if (filters.add(version)) {
            // it's higher then what was there
            if (version == Collections.max(filters)) {
                initWithMaxFilter(version);
            }
        }
    }

    public void removeFilter(Integer version) {
        // the filter was there
        if (filters.remove(version)) {
            // I removed the higher
            Integer max = Collections.max(filters);
            if (version > max) {
                initWithMaxFilter(max);
            }
        }
    }

    private void initWithMaxFilter(Integer version) {
        PdfVersionComboItem currentSelected = (PdfVersionComboItem) getSelectedItem();
        removeAllItems();
        for (PdfVersion current : PdfVersion.values()) {
            if (current.getVersion() > 2 && current.getVersion() >= version) {
                PdfVersionComboItem currentItem = new PdfVersionComboItem(current);
                addItem(currentItem);
                if (currentItem.equals(currentSelected)) {
                    setSelectedItem(currentItem);
                }
            }
        }
        setEnabled(getItemCount() > 1);
    }

    public EventNamespace getEventNamespace() {
        return eventNamespace;
    }

    public void setEventNamespace(EventNamespace eventNamespace) {
        this.eventNamespace = eventNamespace;
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
            RequireUtils.require(version != null, "PDF version cannot be null");
            this.version = version;
        }

        public PdfVersion getVersion() {
            return version;
        }

        public void setSourceVersion(boolean sourceVersion) {
            this.sourceVersion = sourceVersion;
        }

        @Override
        public String toString() {
            String versionString = DefaultI18nContext.getInstance().i18n("Version {0}",
                    Double.toString(version.getVersionAsDouble()));
            if (sourceVersion) {
                return String.format("%s (%s)", versionString,
                        DefaultI18nContext.getInstance().i18n("Same as the input document"));
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
