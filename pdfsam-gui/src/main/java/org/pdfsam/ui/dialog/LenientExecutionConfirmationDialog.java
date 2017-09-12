/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09 feb 2017
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
package org.pdfsam.ui.dialog;

import javax.inject.Inject;

import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;

/**
 * Dialog asking the user if he wants to execute the task leniently
 * 
 * @author Andrea Vacondio
 *
 */
public class LenientExecutionConfirmationDialog extends ConfirmationDialog {
    @Inject
    public LenientExecutionConfirmationDialog(StylesConfig styles) {
        super(styles, DialogStyle.QUESTION, DefaultI18nContext.getInstance().i18n("Yes"),
                DefaultI18nContext.getInstance().i18n("No"));
        this.title(DefaultI18nContext.getInstance().i18n("Task failed"))
                .messageTitle(DefaultI18nContext.getInstance().i18n("PDFsam can try to overcome the failure"))
                .messageContent(DefaultI18nContext.getInstance()
                        .i18n("It may result in PDF files with partial or missing data, proceed anyway?"));
    }
}
