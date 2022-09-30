/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
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
package org.pdfsam.ui.components.dashboard.preference;

import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.ui.components.help.HelpUtils.helpIcon;

import java.util.Comparator;
import java.util.Locale;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.core.support.KeyStringValueItem;
import org.pdfsam.core.support.LocaleKeyValueItem;
import org.pdfsam.ui.components.support.Style;

import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;

/**
 * Preference pane displaying the appearance section
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferenceAppearencePane extends GridPane {

    @Inject
    public PreferenceAppearencePane(@Named("localeCombo") PreferenceComboBox<LocaleKeyValueItem> localeCombo,
            @Named("startupModuleCombo") PreferenceComboBox<KeyStringValueItem<String>> startupModuleCombo,
            ClearStatisticsButton clearStatsButton) {
        I18nContext i18n = I18nContext.getInstance();
        add(new Label(i18n.i18n("Language:")), 0, 0);
        i18n.getSupported().stream().sorted(Comparator.comparing(Locale::getDisplayName))
                .map(LocaleKeyValueItem::new).forEach(localeCombo.getItems()::add);

        localeCombo.setValue(new LocaleKeyValueItem(Locale.getDefault()));
        localeCombo.valueProperty().addListener(
                (observable, oldValue, newValue) -> eventStudio().broadcast(new SetLocaleRequest(newValue.getKey())));
        localeCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(localeCombo, true);
        add(localeCombo, 1, 0);
        add(helpIcon(i18n.i18n("Set your preferred language (restart needed)")), 2, 0);

        add(new Label(i18n.i18n("Startup module:")), 0, 1);
        startupModuleCombo.setMaxWidth(Double.POSITIVE_INFINITY);
        setFillWidth(startupModuleCombo, true);
        add(startupModuleCombo, 1, 1);
        add(helpIcon(i18n.i18n("Set the module to open at application startup (restart needed)")), 2, 1);

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
