/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25 ott 2020
 * Copyright 2019 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.components.dialog;

import static org.pdfsam.i18n.I18nContext.getInstance;

import javax.inject.Inject;

import org.pdfsam.configuration.StylesConfig;

/**
 * Dialog asking for a confirmation before clearing all settings
 * 
 * @author Andrea Vacondio
 *
 */
public class ClearModuleConfirmationDialog extends ConfirmationDialog {
    @Inject
    public ClearModuleConfirmationDialog(StylesConfig styles) {
        super(styles, DialogStyle.QUESTION, i18n().tr("Yes"), i18n().tr("No"));
        this.title(i18n().tr("Confirm clearing")).messageContent(getInstance().i18n("Do you confirm?"));
    }

    /**
     * Sets the dialog message depending on the event setting
     * 
     * @param clearEverything
     * @return
     */
    ConfirmationDialog clearEverything(boolean clearEverything) {
        if (clearEverything) {
            messageTitle(getInstance().i18n("Clear the module settings"));
        } else {
            messageTitle(getInstance().i18n("Clear the selection table"));
        }
        return this;
    }
}
