/*
 * Created on 10/feb/2013
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
package org.pdfsam.gui.view.output;

import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import org.pdfsam.gui.SharedJFileChooser;
import org.pdfsam.gui.view.BaseBrowsableField;
import org.pdfsam.gui.view.Views;
import org.pdfsam.support.filter.FileFilterType;

/**
 * A Panel where the user can select a destination file or directory
 * 
 * @author Andrea Vacondio
 * 
 */
public class DestinationPanel extends JPanel {

    private JCheckBox overwrite = Views.newOverwriteOutputCheckbox();
    private BaseBrowsableField browsableField;

    public DestinationPanel(final FileFilterType filterType, final int chooserMode) {

        browsableField = new BaseBrowsableField() {

            @Override
            protected void onFileSelected(File selected) {
                // nothing
            }

            @Override
            protected JFileChooser getChooser() {
                return SharedJFileChooser.getInstance(filterType, chooserMode);
            }
        };
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        browsableField.setAlignmentX(LEFT_ALIGNMENT);
        add(browsableField);
        overwrite.setAlignmentX(LEFT_ALIGNMENT);
        add(overwrite);
    }

}
