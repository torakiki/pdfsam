/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/giu/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import static java.util.Objects.isNull;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.ui.support.Style;

import javafx.scene.control.RadioButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.VBox;

/**
 * Preference pane displaying the output section
 * 
 * @author Andrea Vacondio
 *
 */
class PreferenceOutputPane extends VBox {

    @Inject
    public PreferenceOutputPane(@Named("smartRadio") PreferenceRadioButton smartRadio) {
        I18nContext i18n = DefaultI18nContext.getInstance();
        ToggleGroup group = new ToggleGroup();

        RadioButton manualRadio = new RadioButton(i18n.i18n("Manually selected"));
        manualRadio.setToggleGroup(group);
        manualRadio.getStyleClass().addAll(Style.VITEM.css());
        manualRadio.setId("manualRadio");

        smartRadio.getStyleClass().addAll(Style.VITEM.css());
        smartRadio.setToggleGroup(group);
        smartRadio.setGraphic(helpIcon(DefaultI18nContext.getInstance()
                .i18n("Automatically set the destination directory to the selected PDF document directory")));
        smartRadio.getStyleClass().addAll(Style.WITH_HELP.css());

        if (isNull(group.getSelectedToggle())) {
            group.selectToggle(manualRadio);
        }

        getChildren().addAll(manualRadio, smartRadio);
        getStyleClass().addAll(Style.CONTAINER.css());
    }
}
