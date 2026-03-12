/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/giu/2014
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
package org.pdfsam.ui.components.prefix;

import javafx.geometry.HPos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.core.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.prefix.Prefix;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Panel with a text field to set the prefix for a task
 *
 * @author Andrea Vacondio
 */
public class PrefixPane extends GridPane
        implements TaskParametersBuildStep<MultipleOutputTaskParametersBuilder<?>>, RestorableView, ResettableView,
        ToolBound {

    private final PrefixField field;
    private final String toolBinding;
    private final PreferencesRepository repository;

    public PrefixPane(String toolBinding, PreferencesRepository repository) {
        this.toolBinding = defaultString(toolBinding);
        this.repository = repository;
        this.field = new PrefixField();
        this.initPrefixValue();
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
        var label = new Label(i18n().tr("Generated PDF documents name prefix:"));
        label.setLabelFor(field);
        GridPane.setValignment(label, VPos.BOTTOM);
        GridPane.setHalignment(label, HPos.LEFT);
        add(label, 0, 0);
        GridPane.setValignment(field, VPos.BOTTOM);
        GridPane.setHalignment(field, HPos.LEFT);
        add(field, 1, 0);
        var helpIcon = helpIcon("""
                %s
                %s
                %s
                """.formatted(i18n().tr("Prefix for the output files name."),
                i18n().tr("Some special keywords are replaced with runtime values."),
                i18n().tr("Right click to add these keywords.")));
        GridPane.setValignment(helpIcon, VPos.CENTER);
        add(helpIcon, 2, 0);

        eventStudio().add(TaskExecutionRequest.class, e -> {
            if (toolBinding.equals(e.toolId())) {
                repository.saveString("DEFAULT_PREFIX", field.getText());
            }
        });
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
        this.initPrefixValue();
    }

    public void initPrefixValue() {
        field.setText(repository.getString("DEFAULT_PREFIX",
                () -> app().persistentSettings().get(StringPersistentProperty.PREFIX).orElse("PDFsam_")));
    }

    @Override
    public String toolBinding() {
        return toolBinding;
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
