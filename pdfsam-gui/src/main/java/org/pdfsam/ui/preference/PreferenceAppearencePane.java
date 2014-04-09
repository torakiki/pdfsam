/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
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
package org.pdfsam.ui.preference;

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
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.LocaleKeyValueItem;
import org.pdfsam.ui.support.Style;

/**
 * Preference pane displaying the appearance section
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
        first.getStyleClass().add("spaced-vitem");

        PreferenceComboBox<KeyStringValueItem<String>> themeCombo = new PreferenceComboBox<>(StringUserPreference.THEME);
        themeCombo.getItems().add(new KeyStringValueItem<>("cornflower.css", "Cornflower"));
        themeCombo.getItems().add(new KeyStringValueItem<>("gray.css", "Gray"));
        themeCombo.getItems().add(new KeyStringValueItem<>("green.css", "Green"));
        themeCombo.getItems().add(new KeyStringValueItem<>("orchid.css", "Orchid"));
        themeCombo.getItems().add(new KeyStringValueItem<>("seagreen.css", "Sea Green"));
        themeCombo.getItems().add(new KeyStringValueItem<>("sienna.css", "Sienna"));
        themeCombo.setTooltip(new Tooltip(i18n.i18n("Set your preferred theme (restart needed)")));
        themeCombo.setValue(new KeyStringValueItem<>(DefaultUserContext.getInstance().getTheme(), ""));
        HBox second = new HBox(2, new Label(i18n.i18n("Theme:")), themeCombo);
        second.setAlignment(Pos.BOTTOM_LEFT);
        second.getStyleClass().add("spaced-vitem");
        getChildren().addAll(first, second);
        getStyleClass().addAll(Style.CONTAINER.css());
    }

}
