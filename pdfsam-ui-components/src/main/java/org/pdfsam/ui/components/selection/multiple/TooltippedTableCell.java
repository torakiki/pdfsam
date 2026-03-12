/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/12/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.scene.control.TableCell;
import javafx.scene.control.Tooltip;

import static java.util.Objects.nonNull;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * A table cell showing a tooltip
 *
 * @author Andrea Vacondio
 */
class TooltippedTableCell<T> extends TableCell<SelectionTableRowData, T> {

    private final Tooltip tooltip = new Tooltip();

    public TooltippedTableCell(String tooltipMessage) {
        requireNotBlank(tooltipMessage, "Tooltiped cell must have a tooltip message");
        this.tooltip.setText(tooltipMessage);
        setAccessibleHelp(tooltipMessage);
    }

    @Override
    public void updateItem(T item, boolean empty) {
        super.updateItem(item, empty);
        if (nonNull(item) && nonNull(tooltip)) {
            setTooltip(tooltip);
        } else {
            setTooltip(null);
        }
        onUpdateItem(item, empty);
    }

    void onUpdateItem(T item, boolean empty) {

    }
}
