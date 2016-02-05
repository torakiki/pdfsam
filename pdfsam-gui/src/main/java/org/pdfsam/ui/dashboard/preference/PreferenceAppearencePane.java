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
package org.pdfsam.ui.dashboard.preference;

import static org.pdfsam.ui.help.HelpUtils.helpIcon;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.LocaleKeyValueItem;
import org.pdfsam.ui.support.Style;

import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;

/**
 * Preference pane displaying the appearance section
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class PreferenceAppearencePane extends GridPane {

    @Inject
    public PreferenceAppearencePane(@Named("localeCombo") PreferenceComboBox<LocaleKeyValueItem> localeCombo,
            @Named("themeCombo") PreferenceComboBox<KeyStringValueItem<String>> themeCombo,
            @Named("startupModuleCombo") PreferenceComboBox<KeyStringValueItem<String>> startupModuleCombo,
            ClearStatisticsButton clearStatsButton) {
        I18nContext i18n = DefaultI18nContext.getInstance();
        add(new Label(i18n.i18n("Language:")), 0, 0);
        for (Locale current : DefaultI18nContext.SUPPORTED_LOCALES) {
            localeCombo.getItems().add(new LocaleKeyValueItem(current));
        }

        localeCombo.setValue(new LocaleKeyValueItem(Locale.getDefault()));
        localeCombo.valueProperty().addListener(
                (observable, oldValue, newValue) -> eventStudio().broadcast(new SetLocaleEvent(newValue.getKey())));
        localeCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(localeCombo, true);
        add(localeCombo, 1, 0);
        add(helpIcon(i18n.i18n("Set your preferred language (restart needed)")), 2, 0);

        add(new Label(i18n.i18n("Theme:")), 0, 1);
        themeCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(themeCombo, true);
        add(themeCombo, 1, 1);
        add(helpIcon(i18n.i18n("Set your preferred theme (restart needed)")), 2, 1);

        add(new Label(i18n.i18n("Startup module:")), 0, 2);
        startupModuleCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(startupModuleCombo, true);
        add(startupModuleCombo, 1, 2);
        add(helpIcon(i18n.i18n("Set the module to open at application startup (restart needed)")), 2, 2);

        GridPane statsPane = new GridPane();
        statsPane.add(clearStatsButton, 0, 0);
        statsPane.add(
                helpIcon(i18n
                        .i18n("Usage statistics are used to populate the modules quick bar on the left with the most used and most recently used modules.")),
                1, 0);
        statsPane.getStyleClass().addAll(Style.GRID.css());
        add(statsPane, 0, 3, 3, 1);
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
    }

}
