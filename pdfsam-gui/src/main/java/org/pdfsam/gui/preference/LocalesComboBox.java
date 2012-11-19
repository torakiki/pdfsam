/*
 * Created on 13/apr/2012
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
package org.pdfsam.gui.preference;

import java.awt.Color;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JComboBox;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.support.StringKeyValueItem;

/**
 * Combo box showing locales and setting the user preference about locale.
 * 
 * @author Andrea Vacondio
 * 
 */
class LocalesComboBox extends JComboBox {

    public LocalesComboBox() {
        initItems();
        setBackground(Color.WHITE);
        setSelectedItem(new StringKeyValueItem(DefaultUserContext.getInstance().getLocale(), ""));
        addItemListener(new ItemListener() {

            @Override
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    DefaultUserContext.getInstance().setStringPreference(StringUserPreference.LOCALE,
                            ((StringKeyValueItem) e.getItem()).getKey());
                }
            }
        });
    }

    private void initItems() {
        addItem(new StringKeyValueItem("ar", "Arabic"));
        addItem(new StringKeyValueItem("ast", "Asturian"));
        addItem(new StringKeyValueItem("bs", "Bosnian"));
        addItem(new StringKeyValueItem("pt_BR", "Brazilian Portuguese"));
        addItem(new StringKeyValueItem("bg", "Bulgarian"));
        addItem(new StringKeyValueItem("ca", "Catalan"));
        addItem(new StringKeyValueItem("hr", "Croatian"));
        addItem(new StringKeyValueItem("cs", "Czech"));
        addItem(new StringKeyValueItem("da", "Danish"));
        addItem(new StringKeyValueItem("nl", "Dutch"));
        addItem(new StringKeyValueItem("en_GB", "English (UK)"));
        addItem(new StringKeyValueItem("fa", "Persian"));
        addItem(new StringKeyValueItem("et", "Estonian"));
        addItem(new StringKeyValueItem("fi", "Finnish"));
        addItem(new StringKeyValueItem("fr", "French"));
        addItem(new StringKeyValueItem("gl", "Galician"));
        addItem(new StringKeyValueItem("de", "German"));
        addItem(new StringKeyValueItem("el", "Greek"));
        addItem(new StringKeyValueItem("iw_IL", "Hebrew"));
        addItem(new StringKeyValueItem("hu", "Hungarian"));
        addItem(new StringKeyValueItem("ja", "Japanese"));
        addItem(new StringKeyValueItem("id", "Indonesian"));
        addItem(new StringKeyValueItem("it", "Italian"));
        addItem(new StringKeyValueItem("ko", "Korean"));
        addItem(new StringKeyValueItem("nb", "Norwegian Bokmal"));
        addItem(new StringKeyValueItem("lv", "Latvian"));
        addItem(new StringKeyValueItem("lt", "Lithuanian"));
        addItem(new StringKeyValueItem("pl", "Polish"));
        addItem(new StringKeyValueItem("pt", "Portuguese"));
        addItem(new StringKeyValueItem("ro", "Romanian"));
        addItem(new StringKeyValueItem("ru", "Russian"));
        addItem(new StringKeyValueItem("zh_CN", "Simplified Chinese"));
        addItem(new StringKeyValueItem("sk", "Slovak"));
        addItem(new StringKeyValueItem("sl", "Slovenian"));
        addItem(new StringKeyValueItem("es", "Spanish"));
        addItem(new StringKeyValueItem("sv", "Swedish"));
        addItem(new StringKeyValueItem("tr", "Turkish"));
        addItem(new StringKeyValueItem("th", "Thai"));
        addItem(new StringKeyValueItem("uk", "Ukrainian"));
        addItem(new StringKeyValueItem("vi", "Vietnamese"));
        addItem(new StringKeyValueItem("zh_TW", "Traditional Chinese"));
        addItem(new StringKeyValueItem("zh_HK", "Chinese (Hong Kong)"));
    }

}
