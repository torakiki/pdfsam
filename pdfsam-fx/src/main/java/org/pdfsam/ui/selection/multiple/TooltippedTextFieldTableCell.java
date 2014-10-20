/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/mar/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.selection.multiple;

import javafx.scene.control.Tooltip;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.util.converter.DefaultStringConverter;

import org.pdfsam.i18n.DefaultI18nContext;

/**
 * Editable cell showing a tooltip
 * 
 * @author Andrea Vacondio
 *
 */
class TooltippedTextFieldTableCell extends TextFieldTableCell<SelectionTableRowData, String> {

    private Tooltip tooltip = new Tooltip(DefaultI18nContext.getInstance().i18n(
            "Double click to set pages you want to merge (ex: 2 or 5-23 or 2,5-7,12-)"));

    TooltippedTextFieldTableCell() {
        super(new DefaultStringConverter());
    }

    @Override
    public void updateItem(String item, boolean empty) {
        super.updateItem(item, empty);
        if (item != null) {
            setTooltip(tooltip);
        } else {
            setTooltip(null);
        }
    }
}
