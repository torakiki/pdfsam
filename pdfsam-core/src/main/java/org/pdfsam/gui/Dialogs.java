/*
 * Created on 15/dic/2011
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui;

import java.awt.Component;

import javax.swing.JOptionPane;

import org.pdfsam.context.DefaultI18nContext;
import org.xnap.commons.i18n.I18n;

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
        I18n i18n = DefaultI18nContext.getInstance().getI18n();
        return JOptionPane.showOptionDialog(
                comp,
                String.format("%s \n%s", i18n.tr("Selected file already exists"),
                        i18n.tr("Would you like to overwrite it?")), filename, JOptionPane.OK_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE, null, null, null);
    }
}
