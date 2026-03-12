/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26/gen/2015
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
package org.pdfsam.tools.split;

import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;
import org.pdfsam.core.support.params.SplitParametersBuilder;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * A {@link RadioButton} showing a combo to select a {@link PredefinedSetOfPages} as split option
 *
 * @author Andrea Vacondio
 */
class SplitAfterPredefinedSetOfPagesRadioButton extends RadioButton
        implements SplitParametersBuilderCreator, RestorableView, ResettableView {

    private final ComboBox<ComboItem<PredefinedSetOfPages>> combo;

    public SplitAfterPredefinedSetOfPagesRadioButton(ComboBox<ComboItem<PredefinedSetOfPages>> combo) {
        super(i18n().tr("Split after"));
        this.setAccessibleHelp(i18n().tr("Select to split the PDF after a predefined set of pages"));
        this.combo = combo;
        combo.getSelectionModel().selectFirst();
        combo.setAccessibleText(i18n().tr("Predefined set of pages to split after"));
        combo.setAccessibleHelp(
                i18n().tr("Choose which predefined set of pages to split after (odd pages, even pages or every page)"));
    }

    @Override
    public void resetView() {
        combo.getSelectionModel().selectFirst();
    }

    @Override
    public SimpleSplitParametersBuilder getBuilder(Consumer<String> onError) {
        ComboItem<PredefinedSetOfPages> selected = combo.getSelectionModel().getSelectedItem();
        if (selected != null) {
            return new SimpleSplitParametersBuilder(selected.key());
        }
        onError.accept(i18n().tr("No page selected"));
        return null;
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        if (isSelected()) {
            data.put("splitAfterPredefined", Boolean.TRUE.toString());
        }
        var selected = combo.getSelectionModel().getSelectedItem();
        if (selected != null) {
            data.put("splitAfterPredefined.combo", defaultString(selected.key().toString()));
        }

    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get("splitAfterPredefined")).map(Boolean::valueOf).ifPresent(this::setSelected);
        Optional.ofNullable(data.get("splitAfterPredefined.combo")).map(PredefinedSetOfPages::valueOf)
                .flatMap(key -> this.combo.getItems().stream().filter(i -> i.key().equals(key)).findFirst())
                .ifPresent(this.combo.getSelectionModel()::select);
    }

    /**
     * Builder for the {@link SimpleSplitParameters}
     *
     * @author Andrea Vacondio
     */
    static class SimpleSplitParametersBuilder extends SplitParametersBuilder<SimpleSplitParameters> {

        private final PredefinedSetOfPages pages;

        SimpleSplitParametersBuilder(PredefinedSetOfPages pages) {
            this.pages = pages;
        }

        @Override
        public SimpleSplitParameters build() {
            SimpleSplitParameters params = new SimpleSplitParameters(pages);
            params.setCompress(isCompress());
            params.setExistingOutputPolicy(existingOutput());
            params.setVersion(getVersion());
            params.setOutput(getOutput());
            params.setOutputPrefix(getPrefix());
            params.addSource(getSource());
            params.setOptimizationPolicy(getOptimizationPolicy());
            params.discardOutline(isDiscardBookmarks());
            return params;
        }
    }
}
