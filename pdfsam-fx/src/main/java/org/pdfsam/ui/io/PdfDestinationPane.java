/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
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
package org.pdfsam.ui.io;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.ui.event.SetDestinationEvent;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.pdf.PdfVersion;

/**
 * Panel letting the user select an output destination for generated Pdf document/s.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfDestinationPane extends DestinationPane implements ModuleOwned {

    private PdfVersionCombo version;
    private PdfVersionConstrainedCheckBox compress;
    private String ownerModule = StringUtils.EMPTY;

    public PdfDestinationPane(BrowsableField destination, String ownerModule) {
        super(destination);
        this.ownerModule = defaultString(ownerModule);
        version = new PdfVersionCombo(ownerModule);
        compress = new PdfVersionConstrainedCheckBox(PdfVersion.VERSION_1_5, ownerModule);
        compress.setText(DefaultI18nContext.getInstance().i18n("Compress output file/files"));
        HBox versionPane = new HBox(2,
                new Label(DefaultI18nContext.getInstance().i18n("Output document pdf version:")), version);
        versionPane.setAlignment(Pos.BOTTOM_LEFT);
        versionPane.getStyleClass().addAll(Style.VITEM.css());
        getChildren().addAll(compress, versionPane);
        eventStudio().addAnnotatedListeners(this);
    }

    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener
    public void setDestination(SetDestinationEvent event) {
        destination().setTextFromFile(event.getFootprint());
    }
}
