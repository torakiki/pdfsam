/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/gen/2015
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
package org.pdfsam.split;

import static org.apache.commons.lang3.StringUtils.defaultString;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.params.SplitParametersBuilder;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.model.parameter.SimpleSplitParameters;
import org.sejda.model.pdf.page.PredefinedSetOfPages;

import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;

/**
 * A {@link RadioButton} showing a combo to select a {@link PredefinedSetOfPages} as split option
 * 
 * @author Andrea Vacondio
 *
 */
class SplitAfterPredefinedSetOfPagesRadioButton extends RadioButton
        implements SplitParametersBuilderCreator, RestorableView {

    private ComboBox<KeyStringValueItem<PredefinedSetOfPages>> combo;

    public SplitAfterPredefinedSetOfPagesRadioButton(ComboBox<KeyStringValueItem<PredefinedSetOfPages>> combo) {
        super(DefaultI18nContext.getInstance().i18n("Split after"));
        this.combo = combo;
        combo.getSelectionModel().selectFirst();
    }

    public SimpleSplitParametersBuilder getBuilder(Consumer<String> onError) {
        KeyStringValueItem<PredefinedSetOfPages> selected = combo.getSelectionModel().getSelectedItem();
        if (selected != null) {
            return new SimpleSplitParametersBuilder(selected.getKey());
        }
        onError.accept(DefaultI18nContext.getInstance().i18n("No page selected"));
        return null;
    }

    public void saveStateTo(Map<String, String> data) {
        if (isSelected()) {
            data.put("splitAfterPredefined", Boolean.TRUE.toString());
        }
        KeyStringValueItem<PredefinedSetOfPages> selected = combo.getSelectionModel().getSelectedItem();
        if (selected != null) {
            data.put("splitAfterPredefined.combo", defaultString(selected.getKey().toString()));
        }

    }

    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get("splitAfterPredefined")).map(Boolean::valueOf).ifPresent(this::setSelected);
        Optional.ofNullable(data.get("splitAfterPredefined.combo")).map(PredefinedSetOfPages::valueOf)
                .map(KeyStringValueItem::keyEmptyValue).ifPresent(this.combo.getSelectionModel()::select);
    }

    /**
     * Builder for the {@link SimpleSplitParameters}
     * 
     * @author Andrea Vacondio
     *
     */
    static class SimpleSplitParametersBuilder extends SplitParametersBuilder<SimpleSplitParameters> {

        private PredefinedSetOfPages pages;

        SimpleSplitParametersBuilder(PredefinedSetOfPages pages) {
            this.pages = pages;
        }

        public SimpleSplitParameters build() {
            SimpleSplitParameters params = new SimpleSplitParameters(pages);
            params.setCompress(isCompress());
            params.setExistingOutputPolicy(existingOutput());
            params.setVersion(getVersion());
            params.setOutput(getOutput());
            params.setOutputPrefix(getPrefix());
            params.setSource(getSource());
            params.setOptimizationPolicy(getOptimizationPolicy());
            params.discardOutline(isDiscardBookmarks());
            return params;
        }
    }
}
