/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import javafx.application.Platform;
import javafx.beans.binding.IntegerExpression;
import javafx.beans.binding.NumberBinding;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.scene.AccessibleAttribute;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.eventstudio.ReferenceStrength;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.selection.RemoveSelectedEvent;
import org.sejda.model.parameter.base.TaskParameters;

import java.util.Map;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
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
    private SimpleIntegerProperty totalValue = new SimpleIntegerProperty(-1);

    public MultipleSelectionPane(String toolBinding, boolean canDuplicate, boolean canMove,
            TableColumnProvider<?>... columns) {
        requireArg(columns.length > 0, "No column has been selected");
        this.toolBinding = defaultString(toolBinding);
        setTop(new SelectionTableToolbar(toolBinding, canMove));
        table = new SelectionTable(toolBinding, canDuplicate, canMove, columns);
        setCenter(table);
    }

    public void showTotalPagesLabel() {
        var total = new Label();
        total.setId("total-label");
        total.setAccessibleRoleDescription(i18n().tr("Number of total pages"));
        totalValue.addListener((observable, oldValue, newValue) -> Platform.runLater(() -> {
            total.setText(i18n().tr("Total pages: {0}", newValue.toString()));
            total.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT);
        }));
        totalValue.set(0);
        setBottom(total);
        eventStudio().add(PdfLoadRequest.class, this::onTableItemsChange, toolBinding(), Integer.MAX_VALUE,
                ReferenceStrength.STRONG);
        eventStudio().add(DuplicateSelectedEvent.class, this::onTableItemsChange, toolBinding(), Integer.MAX_VALUE,
                ReferenceStrength.STRONG);
        eventStudio().add(ClearToolRequest.class, this::onTableItemsChange, toolBinding(), Integer.MAX_VALUE,
                ReferenceStrength.STRONG);
        eventStudio().add(RemoveSelectedEvent.class, this::onTableItemsChange, toolBinding(), Integer.MAX_VALUE,
                ReferenceStrength.STRONG);

    }

    private void onTableItemsChange(Object event) {
        NumberBinding binding = IntegerExpression.integerExpression(new SimpleIntegerProperty(0)).add(0);
        for (var data : table.getItems()) {
            binding = binding.add(data.selectedPages);
        }
        totalValue.unbind();
        totalValue.bind(binding);
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
