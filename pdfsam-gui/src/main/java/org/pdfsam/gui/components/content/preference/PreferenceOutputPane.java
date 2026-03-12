/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/giu/2014
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
package org.pdfsam.gui.components.content.preference;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.GridPane;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.pdf.PdfVersion;

import static java.util.Objects.isNull;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Preference pane displaying the output section
 *
 * @author Andrea Vacondio
 */
class PreferenceOutputPane extends GridPane {

    @Inject
    public PreferenceOutputPane(@Named("smartRadio") PreferenceRadioButton smartRadio,
            @Named("compressionEnabled") PreferenceCheckBox compressionEnabled,
            @Named("overwriteOutput") PreferenceCheckBox overwriteOutput,
            @Named("discardBookmarks") PreferenceCheckBox discardBookmarks,
            @Named("pdfVersionCombo") PreferenceComboBox<ComboItem<PdfVersion>> pdfVersionCombo,
            @Named("prefixField") PreferencePrefixField prefixField) {

        var pdfVersionLabel = new Label(i18n().tr("Default PDF version:"));
        pdfVersionLabel.setLabelFor(pdfVersionCombo);
        add(pdfVersionLabel, 0, 1);
        setFillWidth(pdfVersionCombo, true);
        pdfVersionCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        add(pdfVersionCombo, 1, 1);
        pdfVersionCombo.setAccessibleText(i18n().tr("Default PDF version"));
        add(helpIcon(i18n().tr("Default PDF version for generated PDF files")), 2, 1);

        ToggleGroup group = new ToggleGroup();

        RadioButton manualRadio = new RadioButton(i18n().tr("Manually selected"));
        manualRadio.setToggleGroup(group);
        manualRadio.getStyleClass().addAll(Style.VITEM.css());
        manualRadio.setId("manualRadio");
        manualRadio.setAccessibleHelp(i18n().tr("Manually select the output destination directory"));
        add(manualRadio, 0, 2, 3, 1);

        smartRadio.getStyleClass().addAll(Style.VITEM.css());
        smartRadio.setToggleGroup(group);
        var smartRadioHelpText = i18n().tr(
                "Automatically set the destination directory to the selected PDF document directory");
        smartRadio.setGraphic(helpIcon(smartRadioHelpText));
        smartRadio.getStyleClass().addAll(Style.WITH_HELP.css());
        smartRadio.setAccessibleHelp(smartRadioHelpText);
        add(smartRadio, 0, 3, 3, 1);

        if (isNull(group.getSelectedToggle())) {
            group.selectToggle(manualRadio);
        }

        add(compressionEnabled, 0, 4, 3, 1);
        add(overwriteOutput, 0, 5, 3, 1);
        add(discardBookmarks, 0, 6, 3, 1);

        var prefixLabel = new Label(i18n().tr("Default prefix:"));
        prefixLabel.setLabelFor(prefixField);
        add(prefixLabel, 0, 7);
        setFillWidth(prefixField, true);
        prefixField.setMaxWidth(Double.POSITIVE_INFINITY);
        add(prefixField, 1, 7);
        prefixField.setAccessibleText(i18n().tr("Default prefix"));
        var helpIcon = helpIcon("""
                %s.
                %s
                %s
                """.formatted(
                i18n().tr("Default prefix for output file names, used in tools that generate multiple files"),
                i18n().tr("Some special keywords are replaced with runtime values."),
                i18n().tr("Right click to add these keywords.")));
        add(helpIcon, 2, 7);

        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
    }
}
