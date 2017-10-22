/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
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
package org.pdfsam.ui.dashboard.preference;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.support.Style;
import org.pdfsam.update.UpdateCheckRequest;
import org.sejda.injector.Prototype;

import javafx.scene.control.Button;

/**
 * Button requesting a check for update
 * 
 * @author Andrea Vacondio
 *
 */
@Prototype
class CheckForUpdatesButton extends Button {

    CheckForUpdatesButton() {
        super(DefaultI18nContext.getInstance().i18n("Check for updates now"));
        getStyleClass().addAll(Style.BUTTON.css());
        setOnAction(e -> eventStudio().broadcast(UpdateCheckRequest.INSTANCE));
    }
}
