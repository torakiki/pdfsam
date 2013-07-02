/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/apr/2012
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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.swing.JComboBox;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.gui.view.SortedComboModel;
import org.pdfsam.support.LocaleKeyValueItem;

/**
 * Combo box showing locales and setting the user preference about locale.
 * 
 * @author Andrea Vacondio
 * 
 */
class LocalesComboBox extends JComboBox<LocaleKeyValueItem> {

    public LocalesComboBox() {
        initModel();
        setBackground(Color.WHITE);
        setSelectedItem(new LocaleKeyValueItem(DefaultI18nContext.getInstance().getLocale()));
        addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    DefaultUserContext.getInstance().setStringPreference(StringUserPreference.LOCALE,
                            ((LocaleKeyValueItem) e.getItem()).getKey());
                }
            }
        });
    }

    private void initModel() {
        List<LocaleKeyValueItem> elements = new ArrayList<>();
        for (Locale current : DefaultI18nContext.SUPPORTED_LOCALES) {
            elements.add(new LocaleKeyValueItem(current));
        }
        setModel(new SortedComboModel<>(elements));
    }

}
