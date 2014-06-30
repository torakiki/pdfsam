/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/giu/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import javafx.scene.control.RadioButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.VBox;

import javax.inject.Named;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.ui.support.Style;

/**
 * Preference pane displaying the output section
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class PreferenceOutputPane extends VBox {

    PreferenceOutputPane() {
        I18nContext i18n = DefaultI18nContext.getInstance();
        ToggleGroup group = new ToggleGroup();

        RadioButton manualRadio = new RadioButton(i18n.i18n("Manually selected"));
        manualRadio.setToggleGroup(group);
        manualRadio.getStyleClass().addAll(Style.VITEM.css());

        PreferenceRadioButton smartRadio = new PreferenceRadioButton(BooleanUserPreference.SMART_OUTPUT,
                i18n.i18n("Use the selected PDF document directory as output directory"), DefaultUserContext
                        .getInstance().isUseSmartOutput());
        smartRadio.setTooltip(new Tooltip(i18n
                .i18n("Automatically set the destination directory to the selected PDF document directory")));
        smartRadio.getStyleClass().addAll(Style.VITEM.css());
        smartRadio.setToggleGroup(group);

        getChildren().addAll(manualRadio, smartRadio);
        getStyleClass().addAll(Style.CONTAINER.css());
    }
}
