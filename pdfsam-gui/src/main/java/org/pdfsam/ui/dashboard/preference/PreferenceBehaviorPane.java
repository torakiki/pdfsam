/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.GridPane;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.ui.support.Style;

/**
 * Preference pane displaying the behavior section
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class PreferenceBehaviorPane extends GridPane {

    @Inject
    public PreferenceBehaviorPane(@Named("checkForUpdates") PreferenceCheckBox checkForUpdates,
            @Named("playSounds") PreferenceCheckBox playSounds,
            @Named("newsDisplayPolicy") PreferenceComboBox<KeyStringValueItem<String>> newsDisplayPolicy) {

        add(checkForUpdates, 0, 0, 2, 1);
        add(playSounds, 0, 1, 2, 1);

        newsDisplayPolicy.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Set how often the PDFsam news panel should be opened")));
        add(new Label(DefaultI18nContext.getInstance().i18n("News panel:")), 0, 2);
        setFillWidth(newsDisplayPolicy, true);
        add(newsDisplayPolicy, 1, 2);

        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
    }

}
