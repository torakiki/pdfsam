/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/giu/2013
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
package org.pdfsam.gui.view.selection;

import java.util.Comparator;

import javax.swing.table.TableCellRenderer;

/**
 * Definition of a column in the selection table
 * 
 * @author Andrea Vacondio
 * @param <T>
 *            {@link Comparable} type of the column data
 */
public interface SelectionTableColumn<T> extends Comparator<SelectionTableRowData> {

    String getColumnName();

    Class<T> getColumnClass();

    T getValueFor(SelectionTableRowData data);

    TableCellRenderer getRenderer();
}
