/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.log;

import java.awt.Container;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JTextPane;
import javax.swing.text.DefaultCaret;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.gui.support.Dialogs;
import org.pdfsam.gui.support.SharedJFileChooser;
import org.pdfsam.support.filter.FileFilterType;

import static org.pdfsam.support.io.TextFileWriter.writeContent;

/**
 * {@link JTextPane} with logging purpose methods reacting to log related events.
 * 
 * @author Andrea Vacondio
 * 
 */
public class JTextLogPane extends JTextPane {

    public JTextLogPane() {
        setEditable(false);
        setDragEnabled(true);
        DefaultCaret caret = (DefaultCaret) getCaret();
        caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
        setBorder(BorderFactory.createEmptyBorder());
        AnnotationProcessor.process(this);
    }

    @EventSubscriber
    public void clearTextPane(@SuppressWarnings("unused") ClearLogEvent event) {
        setText("");
    }

    @EventSubscriber
    public void selectAll(@SuppressWarnings("unused") SelectAllEvent event) {
        selectAll();
        requestFocus();
    }

    @EventSubscriber
    public void saveLog(@SuppressWarnings("unused") SaveLogEvent event) {
        JFileChooser fileChooser = SharedJFileChooser.getInstance(FileFilterType.LOG, JFileChooser.FILES_ONLY);
        Container container = this.getTopLevelAncestor();
        if (fileChooser.showSaveDialog(container) == JFileChooser.APPROVE_OPTION) {
            File chosenFile = fileChooser.getSelectedFile();
            if (chosenFile != null) {
                if (chosenFile.exists()) {
                    if (JOptionPane.OK_OPTION != Dialogs.showOverwriteConfirmationDialog(container,
                            chosenFile.getName())) {
                        return;
                    }
                }
                writeContent(getText()).to(chosenFile);
            }
        }
    }
}
