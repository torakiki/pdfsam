/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import javafx.scene.layout.BorderPane;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.sejda.model.parameter.base.TaskParameters;

import java.util.Map;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.sejda.commons.util.RequireUtils.requireArg;

/**
 * Panel holding the selection table and its toolbar. It is constructed specifying the columns for the selection table and it participates to the {@link TaskParameters} build
 * process
 *
 * @author Andrea Vacondio
 */
public class MultipleSelectionPane extends BorderPane implements ToolBound, RestorableView {

    private String toolBinding = StringUtils.EMPTY;
    private final SelectionTable table;

    public MultipleSelectionPane(String toolBinding, boolean canDuplicate, boolean canMove,
            TableColumnProvider<?>... columns) {
        requireArg(columns.length > 0, "No column has been selected");
        this.toolBinding = defaultString(toolBinding);
        setTop(new SelectionTableToolbar(toolBinding, canMove));
        table = new SelectionTable(toolBinding, canDuplicate, canMove, columns);
        setCenter(table);
    }

    @Override
    public String toolBinding() {
        return toolBinding;
    }

    public SelectionTable table() {
        return table;
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        table.saveStateTo(data);
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        table.restoreStateFrom(data);
    }

}
