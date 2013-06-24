/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/feb/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.gui.view.prefix;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.RequireUtils;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * Panel showing output prefix text field
 * 
 * @author Andrea Vacondio
 * 
 */
public final class PrefixField extends JTextField {

    public PrefixField(boolean forSplit) {
        super("pdfsam_");
        JMenu prefixMenu = new JMenu(DefaultI18nContext.getInstance().i18n("Add prefix"));
        prefixMenu.add(new JMenuItem(new InsertPrefixAction("[TIMESTAMP]")));
        prefixMenu.add(new JMenuItem(new InsertPrefixAction("[BASENAME]")));
        if (forSplit) {
            prefixMenu.add(new JMenuItem(new InsertPrefixAction("[FILENUMBER]")));
            prefixMenu.add(new JMenuItem(new InsertPrefixAction("[CURRENTPAGE]")));
            prefixMenu.add(new JMenuItem(new InsertPrefixAction("[BOOKMARK_NAME]")));
            prefixMenu.add(new JMenuItem(new InsertPrefixAction("[BOOKMARK_NAME_STRICT]")));
        }
        JPopupMenu popup = new JPopupMenu();
        popup.add(prefixMenu);
        setComponentPopupMenu(popup);
    }

    /**
     * {@link AbstractAction} inserting a prefix at the caret position.
     */
    private class InsertPrefixAction extends AbstractAction {
        private String prefix;

        InsertPrefixAction(String prefix) {
            RequireUtils.require(isNotBlank(prefix), "Action for a blank prefix cannot be created");
            putValue(Action.NAME, prefix);
            this.prefix = prefix;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            if (isNotBlank(getText())) {
                StringBuilder sb = new StringBuilder(getText());
                if (isNotBlank(getSelectedText())) {
                    sb.replace(getSelectionStart(), getSelectionEnd(), prefix);
                } else {
                    sb.insert(getCaretPosition(), prefix);
                }
                setText(sb.toString());
            } else {
                setText(prefix);
            }
        }
    }

    /**
     * @return a {@link PrefixField} with context menu with not SplitTask items.
     */
    public static PrefixField newSimplePrefixField() {
        return new PrefixField(false);
    }

    /**
     * @return a {@link PrefixField} with a complete context menu.
     */
    public static PrefixField newPrefixFieldForSplit() {
        return new PrefixField(true);
    }
}
