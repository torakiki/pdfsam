/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/giu/2014
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
package org.pdfsam.ui.prefix;

import java.util.function.Consumer;

import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.support.Style;
import org.sejda.model.prefix.Prefix;

/**
 * Panel with a text field to set the prefix for a task
 * 
 * @author Andrea Vacondio
 *
 */
public class PrefixPane extends HBox implements TaskParametersBuildStep<MultipleOutputTaskParametersBuilder<?>> {

    private PrefixField field = new PrefixField();

    public PrefixPane() {
        super(5);
        getStyleClass().addAll(Style.CONTAINER.css());
        VBox.setVgrow(field, Priority.ALWAYS);
        setAlignment(Pos.BOTTOM_LEFT);
        getChildren().addAll(new Label(DefaultI18nContext.getInstance().i18n("Generated pdf documents name prefix:")),
                field);
    }

    public void addMenuItemFor(Prefix... prefixes) {
        field.addMenuItemFor(prefixes);
    }

    public final String getText() {
        return field.getText();
    }

    public void apply(MultipleOutputTaskParametersBuilder<?> builder, Consumer<String> onError) {
        builder.prefix(getText());
    }
}
