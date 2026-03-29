package org.pdfsam.ui.components.io;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26/03/26
 * Copyright 2026 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import javafx.scene.control.ComboBox;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.AddPdfVersionConstraintEvent;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.RemovePdfVersionConstraintEvent;
import org.pdfsam.model.ui.ResettableView;
import org.sejda.model.output.CompressionPolicy;
import org.sejda.model.pdf.PdfVersion;

import static java.util.Objects.requireNonNullElse;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.model.ui.ComboItem.keyWithEmptyValue;

/**
 * Combo box to let the user select the compression policy. It sends event to add or remove PDF version constraints based on the compression policy selected.
 *
 * @author Andrea Vacondio
 */
public class PdfCompressionPolicyCombo extends ComboBox<ComboItem<CompressionPolicy>>
        implements ToolBound, ResettableView {

    private final String toolBinding;

    public PdfCompressionPolicyCombo(String toolBinding) {
        this.toolBinding = requireNonNullElse(toolBinding, "");
        setAccessibleText(i18n().tr("Sets the compression policy for the output files"));
        getItems().add(new ComboItem<>(CompressionPolicy.COMPRESS, i18n().tr("Compress")));
        getItems().add(new ComboItem<>(CompressionPolicy.NEUTRAL, i18n().tr("Neutral")));
        getItems().add(new ComboItem<>(CompressionPolicy.UNCOMPRESS, i18n().tr("Uncompress")));
        valueProperty().addListener((observable, oldValue, newValue) -> {
            if (CompressionPolicy.COMPRESS == newValue.key()) {
                eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_5), toolBinding);
            } else {
                eventStudio().broadcast(new RemovePdfVersionConstraintEvent(PdfVersion.VERSION_1_5), toolBinding);
            }
        });
        resetView();
    }

    @Override
    public String toolBinding() {
        return toolBinding;
    }

    @Override
    public void resetView() {
        setValue(keyWithEmptyValue(CompressionPolicy.NEUTRAL));
        var policy = app().persistentSettings().get(StringPersistentProperty.COMPRESSION_POLICY)
                .map(CompressionPolicy::valueOf).orElse(CompressionPolicy.COMPRESS);
        setValue(keyWithEmptyValue(policy));
    }
}
