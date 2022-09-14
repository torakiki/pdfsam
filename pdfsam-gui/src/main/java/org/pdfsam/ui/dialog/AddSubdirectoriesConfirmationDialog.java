/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30 ago 2019
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
package org.pdfsam.ui.dialog;

import static org.pdfsam.i18n.I18nContext.getInstance;

import javax.inject.Inject;

import org.pdfsam.configuration.StylesConfig;

/**
 * A dialog asking the user if he wants to add PDF files found in the subdirectories of the selected directory
 * 
 * @author Andrea Vacondio
 *
 */
public class AddSubdirectoriesConfirmationDialog extends ConfirmationDialog {

    @Inject
    public AddSubdirectoriesConfirmationDialog(StylesConfig styles) {
        super(styles, DialogStyle.QUESTION, getInstance().i18n("Yes"), getInstance().i18n("No"));
        this.title(getInstance().i18n("Subdirectories"))
                .messageTitle(getInstance().i18n("Subdirectories have been found"))
                .messageContent(getInstance().i18n("Do you want to add PDF files found in subdirectories?"));
    }

}
