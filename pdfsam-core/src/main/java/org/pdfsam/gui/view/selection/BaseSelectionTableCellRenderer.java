/*
 * Created on 18/giu/2013
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
package org.pdfsam.gui.view.selection;

import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * Base class for the selection table cell renderer
 * 
 * @author Andrea Vacondio
 * 
 */
abstract class BaseSelectionTableCellRenderer extends DefaultTableCellRenderer {

    private static final int PADDING = 5;

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
            int row, int column) {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        this.setBorder(BorderFactory.createCompoundBorder(this.getBorder(),
                BorderFactory.createEmptyBorder(PADDING, PADDING, PADDING, PADDING)));
        return this;

    }

    @Override
    protected void setValue(Object value) {
        setText(getStringValue(value));
    }

    abstract String getStringValue(Object value);
}
