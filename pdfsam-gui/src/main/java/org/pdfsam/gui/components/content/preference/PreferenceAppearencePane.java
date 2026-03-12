/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
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
import javafx.scene.layout.GridPane;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.ui.components.support.Style;

import java.util.Comparator;
import java.util.Locale;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Preference pane displaying the appearance section
 *
 * @author Andrea Vacondio
 */
class PreferenceAppearencePane extends GridPane {

    @Inject
    public PreferenceAppearencePane(@Named("localeCombo") PreferenceComboBox<ComboItem<String>> localeCombo,
            @Named("startupToolCombo") PreferenceComboBox<ComboItem<String>> startupTool,
            @Named("themeCombo") PreferenceComboBox<ComboItem<String>> themeCombo,
            @Named("fontFamilyCombo") PreferenceComboBox<ComboItem<String>> fontFamilyCombo,
            @Named("fontSizeCombo") PreferenceComboBox<ComboItem<String>> fontSizeCombo) {
        var languageLabel = new Label(i18n().tr("Language") + ":");
        languageLabel.setLabelFor(localeCombo);
        add(languageLabel, 0, 0);
        i18n().getSupported().stream().map(ComboItem::fromLocale).sorted(Comparator.comparing(ComboItem::description))
                .forEach(localeCombo.getItems()::add);

        localeCombo.setValue(ComboItem.fromLocale(Locale.getDefault()));
        localeCombo.valueProperty().addListener(
                (observable, oldValue, newValue) -> eventStudio().broadcast(new SetLocaleRequest(newValue.key())));
        localeCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(localeCombo, true);
        add(localeCombo, 1, 0);
        localeCombo.setAccessibleText(i18n().tr("Language"));
        add(helpIcon(i18n().tr("Set your preferred language (restart needed)")), 2, 0);

        var startupToolLabel = new Label(i18n().tr("Startup tool") + ":");
        startupToolLabel.setLabelFor(startupTool);
        add(startupToolLabel, 0, 1);
        startupTool.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(startupTool, true);
        add(startupTool, 1, 1);
        startupTool.setAccessibleText(i18n().tr("Startup tool"));
        add(helpIcon(i18n().tr("Set the tool to open at application startup (restart needed)")), 2, 1);

        var themeLabel = new Label(i18n().tr("Theme") + ":");
        themeLabel.setLabelFor(themeCombo);
        add(themeLabel, 0, 2);
        themeCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(themeCombo, true);
        add(themeCombo, 1, 2);
        themeCombo.setAccessibleText(i18n().tr("Theme"));
        add(helpIcon(i18n().tr("Set the application theme")), 2, 2);

        var fontLabel = new Label(i18n().tr("Font") + ":");
        fontLabel.setLabelFor(fontFamilyCombo);
        add(fontLabel, 0, 3);
        fontFamilyCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(fontFamilyCombo, true);
        add(fontFamilyCombo, 1, 3);
        fontFamilyCombo.setAccessibleText(i18n().tr("Font"));
        add(helpIcon(i18n().tr("Sets the application font")), 2, 3);

        var fontSizeLabel = new Label(i18n().tr("Font size") + ":");
        fontSizeLabel.setLabelFor(fontSizeCombo);
        add(fontSizeLabel, 0, 4);
        fontSizeCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(fontSizeCombo, true);
        add(fontSizeCombo, 1, 4);
        fontSizeCombo.setAccessibleText(i18n().tr("Font size"));
        add(helpIcon(i18n().tr("Set the application font size")), 2, 4);

        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
    }

}
