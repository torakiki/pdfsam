/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.gui.preference;

import java.awt.Color;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JComboBox;
import javax.swing.UIManager;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.support.StringKeyValueItem;

/**
 * Combo showing available themes.
 * 
 * @author Andrea Vacondio
 * 
 */
class ThemesComboBox extends JComboBox {

    public ThemesComboBox() {
        initItems();
        setBackground(Color.WHITE);
        setSelectedItem(new StringKeyValueItem(DefaultUserContext.getInstance().getLookAndFeelClass(), ""));
        addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    DefaultUserContext.getInstance().setStringPreference(StringUserPreference.LOOK_AND_FEEL,
                            ((StringKeyValueItem) e.getItem()).getKey());
                }
            }
        });
    }

    private void initItems() {
        addItem(new StringKeyValueItem(UIManager.getCrossPlatformLookAndFeelClassName(), "Java"));
        addItem(new StringKeyValueItem(UIManager.getSystemLookAndFeelClassName(), "System"));
        addItem(new StringKeyValueItem("com.jgoodies.looks.windows.WindowsLookAndFeel", "Windows"));
        addItem(new StringKeyValueItem("com.jgoodies.looks.plastic.PlasticLookAndFeel", "Plastic"));
        addItem(new StringKeyValueItem("com.jgoodies.looks.plastic.Plastic3DLookAndFeel", "Plastic3D"));
        addItem(new StringKeyValueItem("com.jgoodies.looks.plastic.PlasticXPLookAndFeel", "PlasticXP"));
    }
}
