/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/giu/2014
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
package org.pdfsam.ui.prefix;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.ResettableView;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.model.prefix.Prefix;

import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

/**
 * Panel with a text field to set the prefix for a task
 * 
 * @author Andrea Vacondio
 *
 */
public class PrefixPane extends HBox
        implements TaskParametersBuildStep<MultipleOutputTaskParametersBuilder<?>>, RestorableView, ResettableView {
    private PrefixField field = new PrefixField();

    public PrefixPane() {
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.HCONTAINER.css());
        I18nContext ctx = DefaultI18nContext.getInstance();
        getChildren().addAll(new Label(DefaultI18nContext.getInstance().i18n("Generated PDF documents name prefix:")),
                field,
                        helpIcon(new TextFlow(
                                new Text(ctx.i18n("Prefix for the output files name.") + System.lineSeparator()),
                                new Text(ctx.i18n("Some special keywords are replaced with runtime values.")
                                        + System.lineSeparator()),
                        new Text(ctx.i18n("Right click to add these keywords.")))));
    }

    public void addMenuItemFor(Prefix... prefixes) {
        field.addMenuItemFor(prefixes);
    }

    public void addMenuItemFor(String... prefixes) {
        field.addMenuItemFor(prefixes);
    }

    public final String getText() {
        return field.getText();
    }

    @Override
    public void resetView() {
        field.resetView();
    }

    @Override
    public void apply(MultipleOutputTaskParametersBuilder<?> builder, Consumer<String> onError) {
        builder.prefix(getText());
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put(defaultString(getId()) + "prefix", defaultString(field.getText()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        field.setText(Optional.ofNullable(data.get(defaultString(getId()) + "prefix")).orElse(EMPTY));
    }

}
