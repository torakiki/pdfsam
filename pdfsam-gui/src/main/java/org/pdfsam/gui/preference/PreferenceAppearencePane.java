/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
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
package org.pdfsam.gui.preference;

import java.util.Locale;

import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.support.LocaleKeyValueItem;
import org.pdfsam.support.StringKeyValueItem;

/**
 * Preference pane displaying the Appearence section
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferenceAppearencePane extends VBox {

    PreferenceAppearencePane() {
        I18nContext i18n = DefaultI18nContext.getInstance();

        PreferenceComboBox<LocaleKeyValueItem> localeCombo = new PreferenceComboBox<>(StringUserPreference.LOCALE);
        for (Locale current : DefaultI18nContext.SUPPORTED_LOCALES) {
            localeCombo.getItems().add(new LocaleKeyValueItem(current));
        }
        localeCombo.setTooltip(new Tooltip(i18n.i18n("Set your preferred language (restart needed)")));
        localeCombo.setValue(new LocaleKeyValueItem(DefaultI18nContext.getInstance().getLocale()));
        HBox first = new HBox(2, new Label(i18n.i18n("Language:")), localeCombo);
        first.setAlignment(Pos.BOTTOM_LEFT);
        first.getStyleClass().add("preference");

        PreferenceComboBox<StringKeyValueItem> themeCombo = new PreferenceComboBox<>(StringUserPreference.THEME);
        themeCombo.getItems().add(new StringKeyValueItem("blue.css", "Blue"));
        themeCombo.getItems().add(new StringKeyValueItem("green.css", "Green"));
        themeCombo.getItems().add(new StringKeyValueItem("orange.css", "Orange"));
        themeCombo.getItems().add(new StringKeyValueItem("purple.css", "Purple"));
        themeCombo.getItems().add(new StringKeyValueItem("red.css", "Red"));
        themeCombo.getItems().add(new StringKeyValueItem("yellow.css", "Yellow"));
        themeCombo.setTooltip(new Tooltip(i18n.i18n("Set your preferred theme (restart needed)")));
        themeCombo.setValue(new StringKeyValueItem(DefaultUserContext.getInstance().getTheme(), ""));
        HBox second = new HBox(2, new Label(i18n.i18n("Theme:")), themeCombo);
        second.setAlignment(Pos.BOTTOM_LEFT);
        second.getStyleClass().add("preference");
        getChildren().addAll(first, second);
        getStyleClass().add("pdfsam-container");
    }

}
