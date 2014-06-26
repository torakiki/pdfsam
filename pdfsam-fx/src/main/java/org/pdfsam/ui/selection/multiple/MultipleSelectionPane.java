/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.support.RequireUtils.require;
import javafx.scene.layout.BorderPane;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ModuleOwned;
import org.sejda.model.parameter.base.TaskParameters;

/**
 * Panel holding the selection table and its toolbar. It is constructed specifying the columns for the selection table and it participates to the {@link TaskParameters} build
 * process
 * 
 * @author Andrea Vacondio
 */
public abstract class MultipleSelectionPane extends BorderPane implements ModuleOwned {

    private String ownerModule = StringUtils.EMPTY;
    private SelectionTable table;

    public MultipleSelectionPane(String ownerModule, SelectionTableColumn<?>... columns) {
        require(columns.length > 0, "No column has been selected");
        this.ownerModule = defaultString(ownerModule);
        setTop(new SelectionTableToolbar(ownerModule));
        table = new SelectionTable(ownerModule, new SelectionTableColumn<?>[] {
                new LoadingStatusColumn(this.ownerModule), FileColumn.NAME, LongColumn.SIZE, LongColumn.PAGES,
                LongColumn.LAST_MODIFIED, StringColumn.PAGE_SELECTION });
        setCenter(table);
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    protected SelectionTable table() {
        return table;
    }

}
