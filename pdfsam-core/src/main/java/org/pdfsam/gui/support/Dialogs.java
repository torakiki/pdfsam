/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.gui.support;

import java.awt.Component;

import javax.swing.JOptionPane;

import org.pdfsam.context.DefaultI18nContext;

/**
 * Provides static methods to show different kinds of dialogs.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class Dialogs {

    private Dialogs() {
        // hide
    }

    /**
     * Shows a ok/cancel dialog to ask the user about overwriting a file
     * 
     * @param comp
     *            parent component
     * @param filename
     *            the file which will be overwritten
     * @return an integer indicating the option chosen by the user
     */
    public static int showOverwriteConfirmationDialog(Component comp, String filename) {
        return JOptionPane.showOptionDialog(comp, String.format("%s \n%s",
                DefaultI18nContext.getInstance().i18n("Selected file already exists"), DefaultI18nContext.getInstance()
                        .i18n("Would you like to overwrite it?")), filename, JOptionPane.OK_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE, null, null, null);
    }
}
